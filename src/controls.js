window.app.ports.control.subscribe(function (msg) {
    let command = msg.split(' ')[0];
    let params = msg.split(' ').slice(1);
    switch (command) {
        case "load":
            load(params[0]);
            break;
        case "togglePause":
            togglePause();
            break;
        case "play":
            play(params[0]);
            break;
        case "volume":
            changeVolume(params[0]);
            break;
        case "background":
            changeBackground(params[0]);
            break;
    }
});

load = function (link) {
    const audio = document.getElementById("audio");
    audio.src = link;
    audio.load();
};

play = function (link) {
    const audio = document.getElementById("audio");
    load(link);
    audio.play();
};

togglePause = function () {
    const audio = document.getElementById("audio");
    if (audio.paused) {
        audio.play();
    } else {
        audio.pause();
    }
};

changeVolume = function (volume) {
    const audio = document.getElementById("audio");
    audio.volume = volume / 100;
};

changeBackground = function (path) {
    window.requestAnimationFrame(() => {
        const video = document.getElementById("video");
        const still = document.getElementById("still");
        if (video) {
            video.src = path;
            video.play();
        } else {
            still.src = path;
        }
    });
};

updateProgress = function () {
    const audio = document.getElementById("audio");
    window.app.ports.progress.send(audio.currentTime)
};

setInterval(updateProgress, 1000);
