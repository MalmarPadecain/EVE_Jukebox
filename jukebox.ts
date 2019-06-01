class Playlist {
    name: string;
    songs: Array<Song>;
}

class Song {
    name: string;
    link: string;
    duration: string;
}

class State {
    audio: HTMLAudioElement;
    shuffle: Boolean;
    playlist: Playlist;
    it: Iterator<Song>
}

const state = new State;


// Setup

function init() {
    getPlaylist("EVE_Soundtrack");
}

function getPlaylist(name: String) {
    let path = `/Jukebox/playlists/${name}.json`;
    let xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            state.playlist = <Playlist>JSON.parse(xhr.responseText);
            state.it = makeSongIterator(state.playlist);
            generateTable();
        }
    };
    xhr.open("GET", path, true);
    xhr.send();
}

function generateTable() {
    let table = document.getElementById("table");
    for (let song of state.playlist.songs) {
        console.log(song);
        table.innerHTML += `<tr onclick="play('${song.link}')"><td>num</td><td>${song.name}</td><td>${song.duration}</td></tr>`;
    }
}

// Control
function play(link: string) {
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

function nextSong() {
    play(state.it.next().value.link);
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
    init();
});
setInterval(updateProgress, 1000);