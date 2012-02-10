
import pyaudio
import wave
import sys
import os.path
import time

if len(sys.argv) <= 2:
    print "You need to specify two arguments, the wordlist file to use and "
    print "the folder to record the voice into, respectively."
    sys.exit()


# FUNCTION DEFINITIONS
def compute_noise(the_block):
    the_noise = 0
    for i in xrange(0, len(the_block), 2):
        the_value = ord(the_block[i]) | (ord(the_block[i + 1]) << 8)
        if(the_value > 32767):
            the_value = the_value - 65536
        the_noise += the_value if the_value > 0 else the_value * -1
    return the_noise / (len(the_block) / 2)

def read_and_rotate():
    recent_blocks.append(mic.read(block_size))
    recent_noise.append(compute_noise(recent_blocks[-1]))
    if len(recent_blocks) > 10:
        del recent_blocks[0]
        del recent_noise[0]

def last_are_noisy(count):
    for i in xrange(1, count + 1):
        if recent_noise[-i] < noise_threshold:
            return False
    return True

def last_are_silent(count):
    for i in xrange(1, count + 1):
        if recent_noise[-i] > noise_threshold:
            return False
    return True

#def safe_word(text):
#    return re.sub("[^A-Za-z0-9]", "_", text)

# END OF FUNCTION DEFINITIONS

with open(sys.argv[1]) as wordlist_file:
    wordlist = [word.strip() for word in wordlist_file]
    wordlist_file.close()
wordlist = [word.lower() for word in wordlist if word[0] != "#"]
for word in wordlist:
    if wordlist.count(word) > 1:
        print "The wordlist specified contains the word " + repr(word) + " twice."
        sys.exit()

voice_folder = sys.argv[2]
voice_folder = voice_folder.replace("/", os.path.sep)

existing_words = [word[:-4].lower() for word in os.listdir(voice_folder) if word[-4:] == ".wav"]
new_words = [word for word in wordlist if word not in existing_words]
deleted_words = [word for word in existing_words if word not in wordlist]

print "There are " + str(len(new_words)) + " words for you to record: " + " ".join(new_words)
print ""
print str(len(deleted_words)) + " words are no longer in the wordlist: " + " ".join(deleted_words)
print ""
if len(new_words) == 0:
    print "You don't have any unrecorded words, so I'm going to exit. If you wanted "
    print "to re-record some words, delete them from your voice folder first, then "
    print "re-run this program."
    sys.exit()

print "Here's how it works: I'll tell you a word for you to record. NOTE: 0 should "
print "be pronounced as 'zero', not 'oh', and 'am' should be pronounced as 'AY EM', "
print "not 'am', and likewise for 'pm'. Once I've shown you the word, I'll "
print "wait for it to be completely silent, then I'll tell you. You can then "
print "say the word. I'll again wait for it to be completely silent, then I'll "
print "save what you said. If you want to pause at any time, just remain silent "
print "when I ask you to say the next word; I'll wait 5 seconds and then exit. "
print "You can pick up where you left off by running this program again."
print ""
print "Before we start, I need to figure out what silence in your environment "
print "sounds like. Make sure you don't talk, but that any ambient noise sources "
print "(such as fans) are on so I can see what they sound like. When you're ready, "
print "press enter. I'll get an idea of silence for about five seconds and then "
print "tell you. So, press enter when you're ready. But make sure not to talk!"
raw_input()

print "Waiting two seconds for the sound of you pressing enter to pass..."
time.sleep(2)
print "Starting your microphone up..."
block_size = 256
block_multiplier = 4

pa = pyaudio.PyAudio()
mic = pa.open(format=pyaudio.paInt16, channels=1, rate=44100, input=True, output=False, frames_per_buffer=block_size)
mic.start_stream()
print "Awesome, your microphone works. Let's see what silence sounds like..."

silent_blocks = []
for i in xrange(200*block_multiplier):
    silent_blocks.append(mic.read(block_size))

mic.stop_stream()
print "Hang on a second while I analyze what I just listened to..."
silence = 0
for block in silent_blocks:
    for i in xrange(0, len(block), 2):
        value = ord(block[i]) | (ord(block[i + 1]) << 8)
        if(value > 32767):
            value = value - 65536
        silence += value if value > 0 else value * -1
silence = silence / (200 * block_size)
del silent_blocks
    
print "Silence level is " + str(silence)
print ""
print "Awesome, I now know what silence sounds like in your environment. Now I "
print "need to know what it sounds like when you're talking. Press enter, then "
print "start talking about anything you want until I tell you to stop. Make sure "
print "that you talk the entire time, and don't leave any gaps while you're talking. "
print "It'll be about 5 seconds. You could read this message out loud if you want. So,"
print "press enter when you're ready."
raw_input()

print "Waiting two seconds for the sound of you pressing enter to pass..."
time.sleep(2)
print "Now let's see what you sound like when you're talking..."

noisy_blocks = []
mic.start_stream()
for i in xrange(200*block_multiplier):
    noisy_blocks.append(mic.read(block_size))

mic.stop_stream()
print "Hang on a second while I analyze what I just listened to..."
noise = 0
for block in noisy_blocks:
    for i in xrange(0, len(block), 2):
        value = ord(block[i]) | (ord(block[i + 1]) << 8)
        if(value > 32767):
            value = value - 65536
        noise += value if value > 0 else value * -1
noise = noise / (200 * block_size)
del noisy_blocks

print "Noisy level is " + str(noise)
print ""

noise_threshold = silence + ((noise - silence) / 15) 

print "Using threshold " + str(noise_threshold)
print ""
print "Awesome, I now know about what it sounds like when you're talking. Let's"
print "start recording. I'll tell you that a particular word is next. Wait a bit in "
print "silence until I tell you to say the word, then say it. If you don't like "
print "your recording of a particular word, just keep going; you can delete that "
print "word's file in your voice folder late and re-run this program to re-record it."
print "I'll keep doing this over and over again until you've recorded all of the words "
print "in the wordlist file you specified. Again, if you want to pause, just clam up "
print "for five seconds and I'll exit, and you can re-run this program when you want "
print "to resume."
print ""
print "Press enter when you're ready to start."
raw_input()
print "Waiting 3 seconds..."
time.sleep(3)

# Now we start.

silence_before = 3
silence_after = 5

word_count = len(new_words)
for index, current_word in enumerate(new_words):
    if (index % 20) == 0 and index != 0:
        print "Awesome. 20 more words down. Press enter when you're ready to keep going."
        raw_input()
        print "Waiting 3 seconds..."
        time.sleep(3)
    print "Next word is: (" + str(index + 1) + " of " + str(word_count) + ")   --->   " + current_word
    mic.start_stream()
    recent_blocks = []
    recent_noise = []
    while len(recent_blocks) < 3 + silence_before or not last_are_silent(len(recent_blocks)):
        read_and_rotate()
    # It's silent now.
    print "Say the word above."
    read_since_start = 0
    while read_since_start < 200 and not last_are_noisy(5):
        read_and_rotate()
        read_since_start += 1
    if read_since_start >= 200:
        print "You were silent for five seconds. I'll assume this means you want to"
        print "pause. Run this program again when you want to resume. Good-bye!"
        sys.exit()
    recording_buffer = recent_blocks[-(5 + silence_before):]
    while not last_are_silent(10):
        read_and_rotate()
        recording_buffer.append(recent_blocks[-1])
    print "Saving..."
    recording_buffer = recording_buffer[:-(10 - silence_after)]
    mic.stop_stream()
    file_name = os.path.join(voice_folder, current_word + ".wav")
    wave_file = wave.open(file_name, "w")
    wave_file.setnchannels(1)
    wave_file.setsampwidth(2)
    wave_file.setframerate(44100)
    wave_file.writeframes("".join(recording_buffer))
    wave_file.close()

print "Done!"

















