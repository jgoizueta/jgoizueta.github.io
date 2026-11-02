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

## Privacy

beatviz analyses the sound on your phone, while it listens. The audio is not recorded, stored or sent anywhere.

Purchases are handled by Google Play and RevenueCat, which receive purchase information and an anonymous app identifier, never your audio.
