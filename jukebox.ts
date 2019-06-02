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

class SongIterator implements Iterator<Song> {
    readonly songList: Song[];
    private _index: number;

    constructor(songList: Song[]) {
        this.songList = songList;
        this.index = 0;
    }

    get index(): number {
        return this._index;
    }

    set index(newIndex: number) {
        if (newIndex < this.index && newIndex < 0) {
            this._index = this.songList.length - 1;
        } else if (newIndex > this.index && newIndex >= this.songList.length) {
            this._index = 0;
        } else {
            this._index = newIndex;
        }
    }

    public next(back = false): IteratorResult<Song> {
        if (!back) {
            return {
                done: false,
                value: this._next()
            }
        } else {
            return {
                done: false,
                value: this._last()
            }
        }

    }

    _next() {
        this.index++;
        return this.songList[this.index];
    }

    _last() {
        this.index--;
        return this.songList[this.index];
    }
}

const state = new State;


// Setup
function getPlaylist(name: String) {
    let path = `/Jukebox/playlists/${name}.json`;
    let xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            state.playlist = <Playlist>JSON.parse(xhr.responseText);
            state.it = new SongIterator(state.playlist.songs);
            generateTable();
        }
    };
    xhr.open("GET", path, true);
    xhr.send();
}

function generateTable() {
    let table = document.getElementById("table");
    for (let song of state.playlist.songs) {
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

function lastSong() {
    play(state.it.next(true).value.link);
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

document.addEventListener('DOMContentLoaded', (event) => {
    state.audio = <HTMLAudioElement>document.getElementById("audio");
    state.shuffle = false;
    getPlaylist("EVE_Soundtrack");
    state.audio.addEventListener('ended', (event) => {
        nextSong();
    })
});
setInterval(updateProgress, 1000);