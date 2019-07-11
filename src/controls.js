window.app.ports.control.subscribe(function (msg) {
    let command = msg.split(' ')[0];
    let params = msg.split(' ').slice(1);
    switch (command) {
        case "togglePause":
            togglePause();
            break;
        case "play":
            play(params[0]);
            break;
    }
});

play = function (link) {
    const audio = document.getElementById("audio");
    audio.src = link;
    audio.load();
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
