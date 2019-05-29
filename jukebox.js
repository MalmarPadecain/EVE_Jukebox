function play(track) {
    let audio = document.getElementById("audio");

    document.getElementById("title").innerText = track;
    document.getElementById("source").src = track;

    audio.load();
    audio.play()
}

function update() {
    let audio = document.getElementById("audio");
    let progress = document.getElementById("progress");
    progress.innerText = (Math.round(parseFloat(audio.currentTime) / parseFloat(audio.duration) * 100)).toString() + "%";
}

function pause() {
    let audio = document.getElementById("audio");
    if (audio.paused) {
        audio.play();
    } else {
        audio.pause();
    }
}

setInterval(update, 10);