state = {
    shuffle: false,
    playlist: [],
    playing: null
};

function generateTable() {
    let path = "/Jukebox/playlists/EVE_Soundtrack.json";
    let xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let result = xhr.responseText;
            let playlist = JSON.parse(result);
            let table = document.getElementById("table");
            for (let song of playlist.songs) {
                state.playlist.push(song);
                table.innerHTML += `<tr onclick="play('${song.link}')"><td>num</td><td>${song.name}</td><td>${song.duration}</td></tr>`;
            }
        }
    };
    xhr.open("GET", path, true);
    xhr.send();
}

function play(link) {
    let audio = document.getElementById("audio");

    audio.src = link;
    audio.load();
    audio.play()
}

function updateVolume() {
    let audio = document.getElementById("audio");
    let volumeControl = document.getElementById("volume");

    audio.volume = volumeControl.value
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
setInterval(updateProgress, 500);