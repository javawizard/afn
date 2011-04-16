
from utils import rotations

KEY_NAMES = [["A"], ["A#", "Bb"], ["B", "Cb"], ["C", "B#"], ["C#", "Db"],
             ["D"], ["D#", "Eb"], ["E", "Fb"], ["F", "E#"], ["F#", "Gb"],
             ["G"], ["G#", "Ab"]]

SEVENTH_CHORDS = {"major": ["C", "E", "G", "B"],
                  "minor": ["C", "Eb", "G", "Bb"],
                  "dominant": ["C", "E", "G", "Bb"],
                  "diminished": ["C", "Eb", "F#", "A"],
                  "half-diminished": ["C", "Eb", "F#", "Bb"],
                  "minor major": ["C", "Eb", "G", "B"],
                  "augmented major": ["C", "E", "G#", "B"],
                  "augmented": ["C", "E", "G#", "Bb"]}

def key_to_index(key):
    for index, names in enumerate(KEY_NAMES):
        if key in names:
            return index
    raise ValueError("No such key " + key)

def normal_index(index):
    return index % 12

def index_to_key(index):
    return KEY_NAMES[normal_index(index)][0]
    
SEVENTH_CHORD_NUMBERS = dict([
        (name, [
                (key_to_index(key) - key_to_index("C")) % 12
                for key in note_list
                ]) for name, note_list in SEVENTH_CHORDS.items()])

# Map of key indexes to map of seventh chord names to lists of four values,
# each value being that respective note in the seventh, as an index into
# KEY_NAMES
ALL_SEVENTHS = {}

for key_index in range(len(KEY_NAMES)):
    key_map = {}
    for seventh_name, seventh_notes in SEVENTH_CHORD_NUMBERS.items():
        key_map[seventh_name] = [normal_index(key_index + note)
                                  for note in seventh_notes]
    ALL_SEVENTHS[key_index] = key_map

def search_sevenths(first, second, third, fourth):
    """
    Prints out a list of seventh chords and their respective keys that match
    the specified chord. Usually there will only be one result; the general
    exception is when the notes form a diminished seventh, since any particular
    set of notes form a valid diminished seventh in four different keys.
    """
    first, second, third, fourth = (
            key_to_index(first), key_to_index(second),
            key_to_index(third), key_to_index(fourth)
            )
    for key, seventh_map in ALL_SEVENTHS.items():
        for seventh_name, note_list in seventh_map.items():
            if [first, second, third, fourth] in rotations(note_list):
                print "Match: " + KEY_NAMES[key][0].ljust(3) + seventh_name





















