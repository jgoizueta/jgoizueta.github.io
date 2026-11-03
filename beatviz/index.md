---
layout: default
---

# beatviz user guide

beatviz listens to music through your phone's microphone, finds the beat, and lights one bar on each beat. It shows you where the beat is, how fast it is (in beats per minute, BPM), and where each bar of the music starts.

If you're a musician playing along with other musicians or recordings you may find it useful to have a visual reinforcement of the rhythm and avoid losing track of what part of the bar you're in.

It can also be useful for educational purposes for musicians, dancers and anyone learning to hear the beat: counting a waltz, finding the "one" in a song, or checking the tempo of a track.

## Getting started

1. Open beatviz and allow microphone access when asked. beatviz needs the microphone to hear the music.
2. Play some music, on a speaker or another device, loud enough to stand out clearly from the noise in the room.
3. If the music is not 4/4 (most popular music is) you'll need to adjust the number of beats per bar with the **−** and **+** buttons.
4. Press **START**.
5. After a few beats the bars start following the music and the tempo appears, for example **120 BPM**. Give it about 10 seconds to settle on an accurate tempo.

Press **STOP** when you're done. beatviz also stops listening when you leave the app.

## The screen

From top to bottom:

- **The bars.** One bar lights up on each beat, from left to right, then starts again. The lit bar shows its number, so you can count along.
- **The BPM display.** The tempo of the music. It shows `--- BPM` until beatviz has found the beat. On its left is the **1** chip (see [Beat 1](#beat-1)); on its right, a lock icon appears when the tempo is locked (see [TAP](#helping-beatviz-with-tap)).
- **Beats per bar:** the **−** and **+** buttons.
- **START / STOP** and **TAP.**
- **?**: shows the introduction slides again.

## How beatviz follows the music

beatviz predicts each beat a moment before it happens, so the bar lights up on the beat rather than after it. It needs a few beats at the start to lock on, then follows the music steadily. Short mistakes, like a missed beat or a drum fill, don't throw it off.

When the music gets quiet (a break, a soft intro), the BPM display dims. beatviz then keeps the bars going at the same tempo and waits for the music to come back, instead of guessing from noise.

If the tempo of the song changes, beatviz follows it gradually over a few beats.

## Helping beatviz with TAP

Sometimes beatviz follows the wrong beat: twice as fast or half as fast as you'd count, or off-beat. You can show it the beat:

1. Press **TAP** a few times along with the music, on the beat.
2. The bars jump to your beat, and a **lock icon** appears next to the BPM.

While the tempo is locked, beatviz keeps your tempo and beat, and only makes small corrections from what it hears.

To release the lock, either:

- press **TAP** once on its own (after a pause of a couple of seconds), or
- touch the BPM display or the lock icon.

beatviz then follows the music on its own again. TAP works only while beatviz is listening.

## Beat 1

Music is organized in bars (measures) of a few beats each, and the first beat of each bar, the "one", is usually the strongest. beatviz tries to find it by itself, so that bar 1 lights up on the one. This takes longer than finding the tempo: often another 10 seconds or so after the tempo has settled.

The **1** chip next to the BPM shows what's happening:

| Chip | Meaning |
|---|---|
| Gray **1** with an outline | beatviz is still looking for beat 1. |
| White **1** on green | beatviz has found beat 1. |
| Dark **1** on orange | You've set beat 1 yourself. |

To set beat 1 yourself, touch a bar at the moment its beat sounds. For example, touch the first bar right when you hear the first beat of a bar. From then on, that beat is bar 1, and the chip turns orange.

To go back to automatic, press the orange **1** chip.

Finding beat 1 is harder than finding the beat, and beatviz gets it wrong more often, especially in music without a clear strong beat. That's why you can always set it yourself.

## Beats per bar

Use **−** and **+** to choose how many bars are shown, which is the number of beats in each bar of the music:

- **4** for most pop, rock, jazz and dance music;
- **3** for waltzes;
- other values for other meters, or **1** to see just the beat.

beatviz finds beat 1 automatically only when this number matches the music.

## Free version and Pro

The free version has all of beatviz's features. Its only limit is one minute of listening each time you press START; press START again to continue.

beatviz Pro unlocks unlimited listening.

Press **Upgrade** to see the Pro options. If you've already bought Pro, for example on another phone or after reinstalling the app, press **Restore** to get it back.

## Tips for better tracking

- **Play the music loud enough.** beatviz works best when the music is clearly louder than voices or other noise in the room.
- **Use an external microphone.** A USB microphone, connected directly or with a USB adapter, gives noticeably steadier tracking than the phone's built-in microphone. Place it close to the speakers. Bluetooth headsets usually add delay and lower the sound quality, so wired is better.
- **Keep the phone still** and away from things that rattle, and don't cover the microphone.
- **Music with a clear beat works best.** Dance, pop and rock are easy; rubato classical music, free jazz or ambient music may be hard to follow, even for people.
- **If the bars follow the wrong beat**, use TAP.

## How it works

For the curious, here is what happens between the microphone and the bars. Everything runs on the phone.

**1. Sound to features.** The microphone sound is converted to 22,050 samples per second. Every 20 ms (50 times a second), beatviz looks at the last 80 ms of sound and measures how loud it is in 144 frequency bands, from deep bass to high cymbals. It also measures how much each band has just grown louder, which highlights drum hits and note onsets. The result is a list of 288 numbers for each 20 ms frame.

**2. A neural network.** These numbers go through a neural network ([BeatNet+](#credits)) trained on many recordings in which people marked every beat. For each frame it outputs three probabilities that add up to 1: that the frame is a beat, that it is a downbeat (the first beat of a bar), or neither. The network has a memory (LSTM layers), so each answer takes the recent rhythm into account, but it only ever uses sound that has already been heard, which is what lets it work in real time.

**3. A particle filter.** The probabilities are noisy, so they don't decide the beat on their own. A particle filter keeps many guesses at once, each of the form "the tempo is X and we are at this point between two beats". Every frame, each guess moves forward in time; guesses that expect a beat when the network says "beat" become more likely, and guesses that don't become less likely and are replaced by copies of the better ones. When the surviving guesses agree that a beat has arrived, a beat is reported, with the current tempo. A second set of guesses works out where in the bar each beat falls, for bars of 2, 3 or 4 beats, and marks the beats that start a bar.

**4. The display.** The app turns these beats into a steady grid of predicted beats. The grid follows the detections gradually, so a single wrong detection doesn't move it, keeps going through quiet passages, and is shown slightly ahead of time to make up for the moment it takes to hear and analyse each beat. Beat 1 is placed by comparing the downbeats with your beats-per-bar setting, unless you've set it yourself.

## Privacy

beatviz analyses the sound on your phone, while it listens. The audio is not recorded, stored or sent anywhere.

Purchases are handled by Google Play and RevenueCat, which receive purchase information and an anonymous app identifier, never your audio. See the full [privacy policy](privacy/).

## Credits

Beat tracking uses [BeatNet+](https://github.com/Soundbrenner/BeatNet-Plus) by Mojtaba Heydari, Frank Cwitkowitz and Zhiyao Duan, licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). beatviz runs a C++ port of its model on the phone.
