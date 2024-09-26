from typing import Optional
import enum
import sys
import curses
import rope
import curses.ascii


class Mode(enum.Enum):
    NORMAL = 1
    INSERT = 2


ROPE: rope.Node = rope.Node.from_bytes(b"""
A quick brown
fox jumps
over the lazy
dog
and
then
some
other
lines
below
""")

# text visible on screen; computed from the ROPE
LINES: list[bytes] = []

# Infinite linear undo history
HISTORY: list[rope.Node] = [ROPE]

# Number of lines of text visible on screen
N_LINES = 20

# Vim-like mode
MODE: Mode = Mode.NORMAL

# number of the first line of the ROPE that's visible in the view
# (position of visible area relative to the ROPE)
VIEW_FIRST_LINE = 0


def get_text() -> list[bytes]:
    global LINES

    view_first_pos = min(rope.Pos(row=VIEW_FIRST_LINE, col=0), ROPE.end_pos())
    print(f"SPLIT at view first pos which is {view_first_pos}")
    _, view_onwards = ROPE.split_at_pos(view_first_pos)
    view_end_pos = min(view_onwards.end_pos(), rope.Pos(row=N_LINES, col=0))
    view, _ = view_onwards.split_at_pos(view_end_pos)


    res = view._all_bytes()[:-1]
    res_l = res.split(b'\n')
    print(res, file=sys.stderr)
    assert len(res_l) <= N_LINES
    LINES = res_l
    return res_l

def render_text(stdscr):
    global LINES
    stdscr.clear()
    LINES = get_text()
    for i, line in enumerate(LINES):
        stdscr.addstr(i, 0, line.decode())
    stdscr.refresh()

def insert(key, cursor_y, cursor_x):
    global ROPE
    HISTORY.append(ROPE)
    print(f"{key}, '{chr(key)}', row: {cursor_y}, col: {cursor_x}", file=sys.stderr)
    before: rope.Node
    after: rope.Node
    before, after = ROPE.split_at_pos(rope.Pos(cursor_y+VIEW_FIRST_LINE, cursor_x))
    ROPE = before.concat(rope.Node.from_bytes(bytes([key]))).concat(after)
    get_text()

def delete(cursor_y, cursor_x):
    global ROPE
    offset = ROPE.pos2offset(rope.Pos(cursor_y, cursor_x))
    if offset == 0: return
    HISTORY.append(ROPE)
    before, after = ROPE.split_at(offset-1)
    _, after = after.split_at(1)
    ROPE = before.concat(after)
    get_text()

def clamp(x, fr, to):
    assert fr <= to
    return min(max(x, fr), to)


def undo(cursor_y, cursor_x):
    global ROPE
    ROPE = HISTORY[-1]
    if len(HISTORY) > 1:
        HISTORY.pop()
    get_text()
    cursor_y = clamp(cursor_y, 0, len(LINES)-1)
    cursor_x = clamp(cursor_x, 0, len(LINES[cursor_y]))
    return cursor_y, cursor_x

def handle_key(key, cursor_y, cursor_x, max_y, max_x):
    global MODE
    global VIEW_FIRST_LINE
    if key == curses.KEY_LEFT:
        cursor_x -= 1
    elif key == curses.KEY_RIGHT:
        cursor_x += 1
    elif key == curses.KEY_UP:
        cursor_y -= 1
    elif key == curses.KEY_DOWN:
        cursor_y += 1
    elif key == curses.KEY_BACKSPACE:
        prev_line_end: Optional[int] = None
        if 1 <= cursor_y:
            prev_line_end = len(LINES[cursor_y-1])
        delete(cursor_y, cursor_x)
        if cursor_x > 0:
            cursor_x -= 1
        else:
            cursor_y -= 1
            if prev_line_end is not None:
                cursor_x = prev_line_end
            else:
                cursor_x = 0

    elif key == 21:  # CTRL+U
        cursor_y, cursor_x = undo(cursor_y, cursor_x)

    elif 0 <= key < 256:  # Any text CHARACTER
        # Assuming a buffer and cursor update are handled here
        #print(f"keey: {chr(key)}, {key}", file=sys.stderr)
        if MODE == Mode.INSERT and (curses.ascii.isprint(key) or
                                    key == curses.ascii.LF) :
            insert(key, cursor_y, cursor_x)
            if key != curses.ascii.LF:
                cursor_x += 1
            else:
                cursor_y += 1
                cursor_x = 0
        elif MODE == Mode.INSERT and key == 27: # ESCAPE
            MODE = Mode.NORMAL
        elif MODE == Mode.NORMAL:
            if key == ord('i'):
                MODE = Mode.INSERT
            elif key == ord('j'):
                VIEW_FIRST_LINE += 1
                cursor_y -= 1
            elif key == ord('k'):
                if VIEW_FIRST_LINE != 0:
                    VIEW_FIRST_LINE -= 1
                    cursor_y += 1

    cursor_y = clamp(cursor_y, 0, N_LINES)
    cursor_y = clamp(cursor_y, 0, len(LINES)-1)
    cursor_y = clamp(cursor_y, 0, max_y)
    true_max_x = min(max_x, len(LINES[cursor_y]))
    cursor_x = clamp(cursor_x, 0, true_max_x)
    return cursor_y, cursor_x

def main(stdscr):
    curses.curs_set(0)
    stdscr.keypad(True)
    cursor_y, cursor_x = 0, 0

    curses.start_color()
    curses.init_pair(1, curses.COLOR_BLACK, curses.COLOR_WHITE)
    highlight_color = curses.color_pair(1)


    max_y, max_x = stdscr.getmaxyx()
    stdscr.move(cursor_y, cursor_x)
    stdscr.addstr(cursor_y, cursor_x, " ", highlight_color)
    mode_line_str = str(MODE)
    stdscr.addstr(max_y - 1, 0, mode_line_str.ljust(max_x - 1), curses.color_pair(1))

    render_text(stdscr)
    while True:
        key = stdscr.getch()
        cursor_y, cursor_x = handle_key(key, cursor_y, cursor_x, max_y - 1, max_x - 1)
        render_text(stdscr)
        stdscr.move(cursor_y, cursor_x)
        stdscr.addstr(cursor_y, cursor_x, " ", highlight_color)
        mode_line_str = str(MODE)
        stdscr.addstr(max_y - 1, 0, mode_line_str.ljust(max_x - 1), curses.color_pair(1))
        stdscr.refresh()

curses.wrapper(main)
