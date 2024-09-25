import typing
import pprint
import math
from dataclasses import dataclass
import sys
# from collections import namedtuple

class Pos(typing.NamedTuple):
    row: int
    col: int

@dataclass
class NodeInfo:
    num_newlines: int
    num_bytes: int

    # position of last newline relative to the text inside this node:
    # node.text[last_newline_pos] == ord('\n')
    last_newline_pos: typing.Optional[int]

    @classmethod
    def combine(cls, others: typing.Iterable[typing.Self]) -> typing.Self:
        others_l = list(others)
        num_newlines = sum(other.num_newlines for other in others_l)
        num_bytes = sum(other.num_bytes for other in others_l)

        last_newline_pos = None
        bytes_before_last_newline = 0
        for other in others_l:
            if other.last_newline_pos != None:
                last_newline_pos = bytes_before_last_newline + other.last_newline_pos
            bytes_before_last_newline += other.num_bytes

        res = NodeInfo(num_newlines=num_newlines,
                       num_bytes=num_bytes,
                       last_newline_pos=last_newline_pos)
        # print(f"Combiding {len(list(others_l))} infos")
        # for other in others:
        return res

NodeT = typing.TypeVar('NodeT', bound='Node')

@dataclass
class Node:
    # Maybe count other stuff as well?
    info: NodeInfo

    B: typing.ClassVar[int] = 4

    def _debug_check(self, expected_height=None):
        """Slow and recursive!"""
        height = self.height()
        if expected_height is not None:
            assert height == expected_height
        if height == 0:
            assert isinstance(self, Leaf)


        # info = self.info
        text: bytes = self._all_bytes()
        assert self.info.num_bytes == len(text)
        assert text.count(b'\n') == self.info.num_newlines
        if self.info.num_newlines == 0:
            assert self.info.last_newline_pos is None
        else:
            assert self.info.last_newline_pos is not None
            if text.rfind(b'\n') != self.info.last_newline_pos:
                breakpoint()
            assert text.rfind(b'\n') == self.info.last_newline_pos

        if isinstance(self, Inner):
            assert len({type(kid) for kid in self.kids}) == 1
            assert len(self.kids) > 0, "Empty rope should be a Leaf!"
            for kid in self.kids:
                kid._debug_check(height-1)


    def _all_bytes(self) -> bytes:
        match self:
            case Inner(_, kids):
                res = bytearray()
                for kid in kids:
                    res += kid._all_bytes()
                return bytes(res)
            case Leaf(_, b):
                return b
            case _: assert False

    def height(self) -> int:
        raise NotImplementedError("subclass")

    def num_kids_or_bytes(self) -> int:
        match self:
            case Inner(_, kids): return len(kids)
            case Leaf(_, b): return len(b)
            case _: assert False

    @classmethod
    def from_bytes(cls, b: bytes, depth: None|int = None) -> typing.Self:
        """Creates a new Rope from a sequence of bytes"""
        if depth is not None:
            #print(f"Constructing from {len(b)} bytes; target depth: {depth}")
            assert cls.B**(depth+1) >= len(b)

            assert depth >= 0


        # Split the bytes into at most B blocks
        assert cls.B > 1
        if depth is None:
            depth = 0
            sz = cls.B
            while sz < len(b):
                sz *= cls.B
                depth += 1
            #breakpoint()
            #print(f"Depth computed to: {depth} from size {len(b)}")

        if len(b) <= cls.B:
            res = Leaf._leaf_from_bytes(b)
            for _ in range(depth):
                res = Inner.from_kids([res])
            assert res.height() == depth, f"expected: {depth}, actual: {res.height()}"
            return res

        piece_size = max(math.ceil(len(b) / cls.B), cls.B)
        piece_start = 0
        kids: list['Node'] = []
        while piece_start < len(b):
            piece_end = min(len(b), piece_start + piece_size)
            kids.append(
                Node.from_bytes(b[piece_start:piece_end],
                                depth=depth-1)
            )
            piece_start = piece_end

        return Inner.from_kids(kids)

    @classmethod
    def _merge(cls, a: 'Node', b: 'Node') -> 'Node':
        """Merges if preconditions are met: same height, same type,
        combined kid/array sizes fit in B"""
        assert a.num_kids_or_bytes() + b.num_kids_or_bytes() <= cls.B
        match (a, b):
            case (Inner(_, lk), Inner(_, rk)):
                return Inner.from_kids(lk + rk)
            case (Leaf(_, lb), Leaf(_, rb)):
                return Leaf._leaf_from_bytes(lb + rb)
            case x, y:
                assert False, f"Logic error: left, right types should match: {type(x)}, {type(y)}"


    def _concat_lower(self: 'Node', other: 'Node', h_self: int, h_other: int) -> 'Node':
        """Result is self ++ other; nothing is modified.
        """
        assert h_self >= h_other

        # We will be merging `left` with `right`: `left` is the
        # rightmost part of `self`, and `right` is `other` or `other`
        # wrapped. Chain of all rightmost nodes from `self` to `left`,
        # which will be the same height as `other`. Adding an extra
        # 'super-root' to the beginning to not handle special cases
        # later.

        root2left: list['Node'] = [left := self]
        for _ in range(h_self-h_other):
            assert isinstance(left, Inner)
            assert len(left.kids) > 0
            root2left.append(left := left.kids[-1])

        # `root2left` contains all nodes from the top to `left`
        # including `left`; `left` has the same height as other
        if left.height() != h_other:
            breakpoint()
        assert left.height() == h_other


        # Walk up the chain popping off the bottom nodes and wrapping
        # `right` with parents until we find 2 nodes that can be
        # merged. Invariant: root2left[-1] has the same height as
        # `right`, root2left is never empty.
        right: 'Node' = other
        assert root2left
        while root2left[-1].num_kids_or_bytes() + right.num_kids_or_bytes() > self.B:
            right = Inner.from_kids([right])
            if len(root2left) == 1:
                root2left = [Inner.from_kids([root2left[-1]])]
            else:
                root2left.pop()
        assert type(root2left[-1]) == type(right), "Both Inner or both Leaf."

        merged: 'Node' = self._merge(root2left[-1], right)
        *parents, left = root2left

        parents_inner: list[Inner] = []
        for node in parents:
            assert isinstance(node, Inner)
            parents_inner.append(node)

        # Create new rope where rightmost lowest kid has med
        # updated. parents[-1].kids[-1] is replaced with rigth_low_kid
        for par in reversed(parents_inner):
            merged = Inner.from_kids(par.kids[:-1] + [merged])
        return merged



    def _concat_higher(self: 'Node',  other: 'Node', h_self: int, h_other: int) -> 'Node':
        """See concat lower. Too different from concat_lower to make a general version."""
        assert h_self <= h_other

        root2right: list['Node'] = [right := other]
        for _ in range(h_other-h_self):
            assert isinstance(right, Inner)
            assert len(right.kids) > 0
            root2right.append(right := right.kids[0])

        assert right.height() == h_self

        left: 'Node' = self
        assert root2right
        while root2right[-1].num_kids_or_bytes() + left.num_kids_or_bytes() > self.B:
            left = Inner.from_kids([left])
            if len(root2right) == 1:
                root2right = [Inner.from_kids([root2right[-1]])]
            else:
                root2right.pop()
        assert type(root2right[-1]) == type(left), "Both Inner or both Leaf."

        merged: 'Node' = self._merge(left, root2right[-1])
        *parents, right = root2right

        parents_inner: list[Inner] = []
        for node in parents:
            assert isinstance(node, Inner)
            parents_inner.append(node)

        for par in reversed(parents_inner):
            merged = Inner.from_kids([merged] + par.kids[1:])
        return merged

    def concat(self: 'Node', other: 'Node') -> 'Node':
        """Concatenates of this rope with another. Does not modify
        this tree.

        """

        # TODO special case for concat with empty!
        if self.info.num_bytes == 0: return other
        if other.info.num_bytes == 0: return self

        h_self = self.height()
        h_other = other.height()
        #breakpoint()
        if h_self >= h_other:
            return self._concat_lower(other, h_self, h_other)
        else:
            return self._concat_higher(other, h_self, h_other)

        # raise NotImplementedError("""TODO walk from 'pos' upwards along 'chain'; merge nodes, handle overflows.""")


        # raise NotImplementedError("TODO finish")

    def pos2offset(self, p: Pos) -> int:
        """Example: rope.pos2offset(Pos(0, 0)) == 0
        """
        assert p.row <= self.info.num_newlines
        match self:
            case Leaf(_, data):
                row = p.row
                start = 0
                while row > 0:
                    line_break = data.find(b'\n', start)
                    assert line_break != -1, f"Pos {p} outside of '{data}'"
                    start = line_break+1
                    row -= 1
                next_rb = data.find(b'\n', start)
                assert next_rb == -1 or next_rb - start >= p.col, (
                    f"Can't find pos {p}: line break on line {p.row} before pos {p.col}; "
                    f"bytes are: {self._all_bytes()}"
                )
                return start + p.col
            case Inner(_, kids):
                start = Pos(row=0, col=0)
                count_bytes = 0
                for kid in kids:
                    end_col: int
                    if kid.info.num_newlines == 0:
                        end_col = start.col + kid.info.num_bytes
                    else:
                        assert kid.info.last_newline_pos is not None
                        end_col = kid.info.num_bytes - kid.info.last_newline_pos-1
                    end = Pos(row=start.row + kid.info.num_newlines,
                              col = end_col)
                    if start <= p <= end:
                        kid_p: Pos
                        if p.row == start.row:
                            kid_p = Pos(row=0, col=p.col - start.col)
                        else:
                            assert p.row > start.row
                            kid_p = Pos(row=p.row - start.row, col=p.col)
                        return count_bytes + kid.pos2offset(kid_p)
                    start = end
                    count_bytes += kid.info.num_bytes

                raise ValueError(f"invalid pos: {p}, data is: {self._all_bytes()}")
            case _:
                assert False, "Logic error"

    def split_at(self, offset: int) -> tuple['Node', 'Node']:
        assert offset <= self.info.num_bytes
        match self:
            case Leaf(_, data):
                return Node.from_bytes(data[:offset], ), Node.from_bytes(data[offset:])

            case Inner(_, kids):
                start = 0
                for i, kid in enumerate(kids):
                    if start <= offset <= start + kid.info.num_bytes:
                        kid_a, kid_b = kid.split_at(offset - start)

                        a: Node = Inner.from_kids(kids[:i]) if i > 0 else Leaf.from_bytes(b'')
                        b: Node = Inner.from_kids(kids[i+1:]) if kids[i+1:] else Leaf.from_bytes(b'')
                        return a.concat(kid_a), kid_b.concat(b)
                    start += kid.info.num_bytes
                raise ValueError(f"Invalid split offset: {offset} for data {self._all_bytes()}")
            case _:
                assert False, "Logic error"

    def split_at_pos(self, pos: Pos) -> tuple['Node', 'Node']:
        return self.split_at(self.pos2offset(pos))

    def end_pos(self) -> Pos:
        """TODO test this!

        Splitting by end_pos should give self and ''.
        """
        end_row = self.info.num_newlines
        end_col = (self.info.num_bytes
                   if self.info.last_newline_pos is None
                   else self.info.num_bytes - self.info.last_newline_pos-1
                   )
        res = Pos(row=end_row, col=end_col)
        #print(f"end pos of {self._all_bytes()} is {res}")
        return res


# leaf.from_bytes(b"The quick\nbrown fox\njumps over the\n lazy dog.\n")


@dataclass
class Inner(Node):
    kids: list[Node]



    def height(self) -> int:
        assert len(self.kids) > 0
        return 1 + self.kids[0].height()


    @classmethod
    def from_kids(cls, kids: list[Node]) -> typing.Self:
        assert len(kids) <= cls.B
        if len({kid.height() for kid in kids}) != 1:
            breakpoint()
        assert len({kid.height() for kid in kids}) == 1
        res = Inner(
            info = NodeInfo.combine(k.info for k in kids),
            kids=kids
        )
        res._debug_check()
        return res

@dataclass
class Leaf(Node):
    data: bytes

    def height(self) -> int:
        return 0

    @classmethod
    def _leaf_from_bytes(cls, b: bytes, bypass_check_for_debug=False) -> typing.Self:
        if not bypass_check_for_debug:
            assert len(b) <= cls.B
        return Leaf(
                data = b,
                info=NodeInfo(
                    num_newlines = b.count(b'\n'),
                    last_newline_pos = None if (pos := b.rfind(b'\n')) == -1 else pos,
                    num_bytes=len(b)
                )
            )

# n = Node.from_bytes(b"sli\nghtly longer \ntext over 16 chars\nI think")

def assert_concats():
    for (text1, text2) in [(b"h\n\n", b""),
                           (b"h\n\np", b"hepp"),
                           (b"abcdef", b""),
                           (b"abcdef", b"ghi\nklmn"),
                           (b"sli\nghtly longer \ntext over 16 chars\nI think", b""),
                           (b"slightly longer te\nxt over 16 chars I\nthink", b"\nchr"),
                           (b"slightly longer text ov\ner 16 chars I\nthink", b"under\n16\nchars"),
                           (b"slightly longer text over 16 ch\nars I t\nink",
                            b"another longer text over 16 ch\nars")
    ]:
        n1 = Node.from_bytes(text1)
        n2 = Node.from_bytes(text2)

        n1._debug_check()
        n2._debug_check()
        assert n1.height() >= n2.height()
        n12 = n1.concat(n2)
        n21 = n2.concat(n1)
        assert n12._all_bytes() == text1 + text2
        assert n21._all_bytes() == text2 + text1
        n12._debug_check()
        n21._debug_check()
        print('.', end='')
    print("\nAll concat tests passed")


PosSplit = tuple[Pos, bytes, bytes]
def pos_iterator(b: bytes) -> typing.Generator[PosSplit, None, None]:
    lines: list[bytes] = [l + b'\n' for l in b.split(b'\n')]
    # pre = b''
    for i, line in enumerate(lines):
        for line_idx in range(len(line)):
            pos = Pos(row=i, col=line_idx)
            pre = b''.join(lines[:i]) + line[:line_idx]
            post = line[line_idx:] + b''.join(lines[i+1:])
            yield pos, pre, post[:-1]

def assert_splits():
    for text in [
            b"",
            b"e\nh",
            b"\n\ne\n",
            b"hej\nkom\noch\nhjalp\nmig",
            b"01234",
            b"a\nbit\nlonger\nthan\n16\nchars\nprobably",
            b"""
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
"""
    ]:
        rope = Node.from_bytes(text)
        for pos, pre, post in pos_iterator(text):
            offset = rope.pos2offset(pos)
            assert offset == len(pre)

            #print(pos, pre, post)
            print('.', end='')
            a, b = rope.split_at_pos(pos)
            assert a._all_bytes() == pre, f"expected: {pre}, actual: {a._all_bytes()}"
            assert b._all_bytes() == post, f"expected: {post}, actual: {b._all_bytes()}"

            ax, ay = a.split_at(a.pos2offset(a.end_pos()))
            assert ax._all_bytes() == pre
            assert ay._all_bytes() == b''
            bx, by = b.split_at(b.pos2offset(b.end_pos()))
            assert bx._all_bytes() == post
            assert by._all_bytes() == b''
    print("\nAll split tests pass!")

if __name__ == '__main__':
    ROPE = Node.from_bytes(b"""
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

    assert_concats()
    assert_splits()

# fat_leaf = Leaf._leaf_from_bytes(text, bypass_check_for_debug=True)
# normal_node = Node.from_bytes(text)
# small_node = Node.from_bytes(text2)
# concat = normal_node.concat(small_node)
# pprint.pprint(concat)
# print(concat.all_bytes().decode('utf8'))
# # pprint.pprint(normal_node)
# print(text.decode('utf-8'))
# a, b = normal_node.split_at(Pos(row=2, col=0))
# pprint.pprint(a)
# pprint.pprint(b)
