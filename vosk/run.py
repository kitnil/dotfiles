import json, pyaudio
from vosk import Model, KaldiRecognizer
import subprocess

model = Model("small_model")
rec = KaldiRecognizer(model, 16000)
p = pyaudio.PyAudio()
stream = p.open(
    format=pyaudio.paInt16, channels=1, rate=16000, input=True, frames_per_buffer=8000
)
stream.start_stream()


def listen():
    while True:
        data = stream.read(4000, exception_on_overflow=True)
        if (rec.AcceptWaveform(data)) and (len(data) > 0):
            answer = json.loads(rec.Result())
            if answer["text"]:
                yield answer["text"]


def tts(string):
    subprocess.run(["rm", "-f", "/tmp/vosk-tts.wav"])
    subprocess.run(
        [
            "/nix/store/y73im0yiraa1g3zk6ycks6gjxbqxy5p1-vosk-tts-0.3.54/bin/vosk-tts",
            "-n",
            "vosk-model-tts-ru-0.6-multi",
            "--input",
            string,
            "-o",
            "/tmp/vosk-tts.wav",
        ]
    )
    subprocess.run(["mpv", "--keep-open=no", "/tmp/vosk-tts.wav"])


def main():
    for text in listen():
        if "компьютер" in text and "вкл" in text:
            if "корич" in text and "шум" in text:
                tts("коричневый шум включен")
                subprocess.run(
                    [
                        "mpv",
                        "--no-resume-playback",
                        "--loop",
                        "/srv/video/metube/Smoothed Brown Noise.webm",
                    ]
                )
            if "монитор":
                subprocess.run(
                    [
                        "swaymsg", "output DP-1 dpms on"
                    ]
                )
        if "компьютер" in text and "выкл" in text:
            if "монитор":
                subprocess.run(
                    [
                        "swaymsg", "output DP-1 dpms off"
                    ]
                )

        if (
            "компьютер" in text
            and "открой" in text
            and "поиск" in text
            and "кластер" in text
        ):
            subprocess.run(["firefox", "https://opensearch-dashboards.corp1.majordomo.ru/"])

        if "компьютер" in text and "перекл" in text and "звук" in text:
            subprocess.run(
                [
                    "pactl",
                    "set-sink-mute",
                    "alsa_output.pci-0000_30_00.6.analog-stereo",
                    "toggle",
                ]
            )
            tts("звук переключен")

        if "компьютер" in text and "тиш" in text and "звук" in text:
            subprocess.run(
                [
                    "pactl",
                    "set-sink-volume",
                    "alsa_output.pci-0000_30_00.6.analog-stereo",
                    "-5%",
                ]
            )

        if "компьютер" in text and "гром" in text and "звук" in text:
            subprocess.run(
                [
                    "pactl",
                    "set-sink-volume",
                    "alsa_output.pci-0000_30_00.6.analog-stereo",
                    "+5%",
                ]
            )

        if "компьютер" in text and "умен" in text and "яркость" in text:
            subprocess.run(
                [
                    "brightness", "decrease", "5"
                ]
            )

        if "компьютер" in text and "увел" in text and "яркость" in text:
            subprocess.run(
                [
                    "brightness", "increase", "5"
                ]
            )

        if "компьютер" in text and "напиш" in text:
            subprocess.run(
                [
                    "wtype", " ".join(text.split(" ")[2:])
                ]
            )

        if "компьютер" in text and "сотри" in text:
            subprocess.run(
                [
                    "wtype", "-M", "ctrl", "a", "-m", "ctrl"
                ]
            )
            subprocess.run(
                [
                    "wtype", "-k", "Delete"
                ]
            )

        if "компьютер" in text and "полный" in text and "экран" in text:
            subprocess.run(
                [
                    "wtype", "-M", "win", "f", "-m", "win"
                ]
            )

        if "компьютер" in text and "курсор" in text and "лев" in text:
            subprocess.run(
                [
                    "wtype", "-M", "win", "h", "-m", "win"
                ]
            )

        if "компьютер" in text and "курсор" in text and "прав" in text:
            subprocess.run(
                [
                    "wtype", "-M", "win", "l", "-m", "win"
                ]
            )

        print(text)

if __name__ == '__main__':
    main()
