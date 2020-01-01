window.app.ports.control.subscribe(function ({message, payload}) {
    switch (message) {
        case "load":
            load(payload);
            break;
        case "togglePause":
            togglePause();
            break;
        case "play":
            play(payload);
            break;
        case "volume":
            changeVolume(payload);
            break;
        case "background":
            changeBackground(payload);
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
