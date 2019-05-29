function generateTable() {
    let path = "/Jukebox/playlists/EVE_Soundtrack.json";
    let xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let result = xhr.responseText;
            let playlist = JSON.parse(result);
            let table = document.getElementById("table");
            for (let song of playlist.songs) {
                table.innerHTML += `<tr onclick="play('${song.link}')"><td>num</td><td>${song.name}</td><td>${song.duration}</td></tr>`;
            }
        }
    };
    xhr.open("GET", path, true);
    xhr.send();
}

function play(track) {
    let audio = document.getElementById("audio");

    audio.src = track;
    audio.load();
    audio.play()
}

function update() {
    updateProgress();
    updateVolume();
}

function updateVolume() {
    let audio = document.getElementById("audio");
    let volumeControll = document.getElementById("volume");

    audio.volume = volumeControll.value
}

function updateProgress() {
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

generateTable();
setInterval(update, 500);