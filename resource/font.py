import string

from PIL import ImageFont

WIDTH_DICT = dict()

supported_chars = [c for c in string.printable if not c.isspace() or c == ' ']


font_file_path = "/urs/share/fonts/liberation/LiberationSans-Regular.ttf"
font = ImageFont.truetype(font_file_path, 500)

for char in supported_chars:
    left, _, right, _ = font.getbbox(char)
    width = right - left
    WIDTH_DICT[char] = width
    

AVERAGE_WIDTH = sum(WIDTH_DICT.values()) / len(WIDTH_DICT)

print(f'{WIDTH_DICT=}')
print(f'{AVERAGE_WIDTH=}')

def get_width_liberation_sans(string, size) :
  return sum(WIDTH_DICT.get(s, AVERAGE_WIDTH) for s in string) * size / 500

print(get_width_liberation_sans("The quick brown fox jumps over the lazy dog.", 15))
