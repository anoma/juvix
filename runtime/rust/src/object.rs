// Object access and manipulation

use super::defs::*;
use super::memory::*;

const UID_SHIFT: usize = 0;
const UID_MASK: Word = 0xff;
const NFIELDS_SHIFT: usize = 8;
const NFIELDS_MASK: Word = 0xffffff;

pub fn get_header_uid(h: Word) -> Word {
    (h >> UID_SHIFT) & UID_MASK
}

pub fn get_header_nfields(h: Word) -> usize {
    to_usize((h >> NFIELDS_SHIFT) & NFIELDS_MASK)
}

pub fn make_header(uid: Word, nfields: usize) -> Word {
    (uid << UID_SHIFT) | (to_word(nfields) << NFIELDS_SHIFT)
}

impl Memory {
    pub fn get_header(self: &Memory, ptr: Pointer) -> Word {
        self[to_usize(ptr)]
    }

    pub fn get_field(self: &Memory, ptr: Pointer, idx: Word) -> Word {
        self[to_usize(ptr) + OBJECT_HEADER_SIZE + to_usize(idx)]
    }

    pub fn get_uid(self: &Memory, ptr: Pointer) -> Word {
        get_header_uid(self.get_header(ptr))
    }

    pub fn get_nfields(self: &Memory, ptr: Pointer) -> usize {
        get_header_nfields(self.get_header(ptr))
    }
}
