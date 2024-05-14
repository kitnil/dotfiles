import json, pyaudio
from vosk import Model, KaldiRecognizer
import subprocess
import pathlib
import hashlib
from multiprocessing import Process
import logging
import os

logging.basicConfig()
log = logging.getLogger("vosk")
log_level = os.getenv("VOSK_LOG_LEVEL", "WARNING")
log.info(f"{log_level}: log_level")
log.setLevel(log_level)

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

vosk_tts_cache_directory = pathlib.Path.home().joinpath(".cache", "vosk-tts")
if not vosk_tts_cache_directory.is_dir():
    os.mkdir(vosk_tts_cache_directory)


def mpv(cache_file_string, cache_preserve=True):
    log.debug(f"cache_file: {cache_file_string}")
    subprocess.run(["mpv", "--keep-open=no", cache_file_string])
    cache_file = pathlib.Path(cache_file_string)
    if not cache_preserve:
        cache_file.unlink(missing_ok=True)


def tts(string, cache_preserve=True):
    cache_file = vosk_tts_cache_directory.joinpath(hashlib.sha256(string.encode()).hexdigest() + ".wav")
    if not cache_file.is_file():
        subprocess.run(
            [
                "/nix/store/y73im0yiraa1g3zk6ycks6gjxbqxy5p1-vosk-tts-0.3.54/bin/vosk-tts",
                "-n",
                "vosk-model-tts-ru-0.6-multi",
                "--input",
                string,
                "-o",
                cache_file,
            ]
        )
    cache_file_string = cache_file._str
    process = Process(target=mpv, args=(cache_file_string,cache_preserve,), daemon=True)
    process.start()


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
                tts("монитор включен")
                subprocess.run(
                    [
                        "swaymsg", "output DP-1 dpms on"
                    ]
                )
        if "компьютер" in text and "выкл" in text:
            if "монитор":
                tts("монитор выключен")
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
            tts("звук переключен")
            subprocess.run(
                [
                    "pactl",
                    "set-sink-mute",
                    "alsa_output.pci-0000_30_00.6.analog-stereo",
                    "toggle",
                ]
            )

        if "компьютер" in text and "тиш" in text and "звук" in text:
            tts("тише звук")
            subprocess.run(
                [
                    "pactl",
                    "set-sink-volume",
                    "alsa_output.pci-0000_30_00.6.analog-stereo",
                    "-5%",
                ]
            )

        if "компьютер" in text and "гром" in text and "звук" in text:
            tts("громче звук")
            subprocess.run(
                [
                    "pactl",
                    "set-sink-volume",
                    "alsa_output.pci-0000_30_00.6.analog-stereo",
                    "+5%",
                ]
            )

        if "компьютер" in text and "умен" in text and "яркость" in text:
            tts("умешить яркость")
            subprocess.run(
                [
                    "brightness", "decrease", "5"
                ]
            )

        if "компьютер" in text and "увел" in text and "яркость" in text:
            tts("увеличить яркость")
            subprocess.run(
                [
                    "brightness", "increase", "5"
                ]
            )

        if "компьютер" in text and "напиш" in text:
            tts("пишу текст")
            subprocess.run(
                [
                    "wtype", " ".join(text.split(" ")[2:])
                ]
            )

        if "компьютер" in text and "нажми" in text:
            tts("нажимаю")
            if "пробел" in text:
                subprocess.run(
                    [
                        "wtype", "-k", "space"
                    ]
                )

        if "компьютер" in text and "сотри" in text:
            tts("стираю текст")
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

        if "компьютер" in text and "полный" in text and "кран" in text:
            tts("полный экран")
            subprocess.run(
                [
                    "wtype", "-M", "win", "f", "-m", "win"
                ]
            )

        if "компьютер" in text and "курсор" in text and "лев" in text:
            tts("курсор влево")
            subprocess.run(
                [
                    "wtype", "-M", "win", "h", "-m", "win"
                ]
            )

        if "компьютер" in text and "курсор" in text and "прав" in text:
            tts("курсор вправо")
            subprocess.run(
                [
                    "wtype", "-M", "win", "l", "-m", "win"
                ]
            )

        log.info(text)


if __name__ == '__main__':
    main()
