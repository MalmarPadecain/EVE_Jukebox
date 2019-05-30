class Playlist {
    name: String;
    songs: Array<Song>;
}

class Song {
    name: String;
    link: String;
    duration: String;
}

class State {
    audio: HTMLAudioElement;
    shuffle: Boolean;
    playlist: Playlist;
}

const state = new State;

function generateTable() { //probably needs to be rewritten. looks ugly
    let path = "/Jukebox/playlists/EVE_Soundtrack.json";
    let xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let result = xhr.responseText;
            state.playlist = <Playlist>JSON.parse(result);
            let table = document.getElementById("table");
            for (let song of state.playlist.songs) {
                table.innerHTML += `<tr onclick="play('${song.link}')"><td>num</td><td>${song.name}</td><td>${song.duration}</td></tr>`;
            }
        }
    };
    xhr.open("GET", path, true);
    xhr.send();
}

function play(link) {
    state.audio.src = link;
    state.audio.load();
    state.audio.play()
}

function updateVolume() {
    let volumeControl = <HTMLInputElement>document.getElementById("volume");

    state.audio.volume = parseFloat(volumeControl.value);
}

function updateProgress() {
    let progress = document.getElementById("progress");
    progress.innerText = (Math.round(state.audio.currentTime / state.audio.duration * 100)).toString() + "%";
}

function togglePause() {
    if (state.audio.paused) {
        state.audio.play();
    } else {
        state.audio.pause();
    }
}

function toggleShuffle() {
    state.shuffle = !state.shuffle;
}

function* makeSongIterator(playlist: Playlist) {
    while (true) {
        if (state.shuffle) {
            let index = Math.floor(Math.random() * playlist.songs.length);

            yield playlist[index];
        } else {
            for (let song of playlist.songs) {
                yield song;
            }
        }
    }
}

document.addEventListener('DOMContentLoaded', (event) => {
    state.audio = <HTMLAudioElement>document.getElementById("audio");
    state.shuffle = false;
    generateTable();
});
setInterval(updateProgress, 1000);