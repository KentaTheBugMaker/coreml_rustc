#![no_std]
#![no_main]

//! WASM Memory allocator at least require 2 pages of wasm memory for work.
//!
//! memory used management block(MUMB) is 1KiB for 4 bytes.
//! memory used management block(MUMB) is 512B for 8 bytes.
//! memory used management block(MUMB) is 256B for 16 bytes.
//! memory size and used management block(MSUMB) for largerer sizes. size is recoded by ceil (lb size)
//! each bit of MMB mark used or not.
//!

use core::mem::transmute;

static mut MUMB32: [u8; 1024] = [0; 1024]; // 1KiB + 4*8132B = 33KiB
static mut MUMB64: [u8; 512] = [0; 512]; //  512B + 8*4096B = 32.5KiB
static mut MUMB128: [u8; 256] = [0; 256]; // 256B + 16*2048B = 32.25KiB
static mut MSUMB_LARGE: [u8; 256] = [0; 256]; //256B 256 entries. each entry contains used mark 0x8000 | size of lb size

// # layout
// MMB32 1k
// MMB64 512
// MMB128 256
// MSUMB_LARGE 256
// 4byte block 32k 34k
// 8byte block 32k 66k
// 16byte block 32k 98k
// larger block unknown

// size of total MUMB and MMUSMB is 2KiB

pub fn add(left: usize, right: usize) -> usize {
    left + right
}
#[no_mangle]
pub extern "C" fn malloc(size: usize) -> *mut () {
    //メモリセルの大きさを計算
    let cell_size = size.next_power_of_two().max(4);
    //貸し出せるブロックを検索
    match cell_size {
        4 => {
            //空のブロックがある領域を探す.
            if let Some((chunk, chunk_usedmap)) =
                unsafe { MUMB32.iter().enumerate().find(|x| *x.1 != 255) }
            {
                //0になっているビットを探す.
                let mut mask = 0x01;
                let mut offset = 0;
                while (mask & chunk_usedmap) == mask {
                    mask <<= 1;
                    offset += 1;
                }
                // 0になっているビットを見つけたのでここをマークする.
                unsafe { MUMB32[chunk] |= mask };
                // アドレス生成
                let offset_from_page = 4 * (chunk + offset) + 4096;
                unsafe { transmute(offset_from_page) }
            } else {
                unsafe { transmute(0usize) }
            }
        }
        8 => {
            //空のブロックがある領域を探す.
            if let Some((chunk, chunk_usedmap)) =
                unsafe { MUMB64.iter().enumerate().find(|x| *x.1 != 255) }
            {
                //0になっているビットを探す.
                let mut mask = 0x01;
                let mut offset = 0;
                while (mask & chunk_usedmap) == mask {
                    mask <<= 1;
                    offset += 1;
                }
                // 0になっているビットを見つけたのでここをマークする.
                unsafe { MUMB32[chunk] |= mask };
                // アドレス生成
                let offset_from_page = 8 * (chunk + offset) + 4096 + 2 ^ 15;
                unsafe { transmute(offset_from_page) }
            } else {
                unsafe { transmute(0usize) }
            }
        }
        16 => {
            //空のブロックがある領域を探す.
            if let Some((chunk, chunk_usedmap)) =
                unsafe { MUMB128.iter().enumerate().find(|x| *x.1 != 255) }
            {
                //0になっているビットを探す.
                let mut mask = 0x01;
                let mut offset = 0;
                while (mask & chunk_usedmap) == mask {
                    mask <<= 1;
                    offset += 1;
                }
                // 0になっているビットを見つけたのでここをマークする.
                unsafe { MUMB32[chunk] |= mask };
                // アドレス生成
                let offset_from_page = 16 * (chunk + offset) + 4096 + 2 ^ 16;
                unsafe { transmute(offset_from_page) }
            } else {
                unsafe { transmute(0usize) }
            }
        }
        x => {
            if let Some(entry) = unsafe {
                let mut used_bytes = 0;
                MSUMB_LARGE
                    .iter()
                    .enumerate()
                    .map(|(index, entry)| {
                        let lb_size_of_entry = entry & 0x3f;
                        let size_of_entry = 1usize << lb_size_of_entry;
                        used_bytes += size_of_entry;
                        (index, entry, used_bytes)
                    })
                    .find(|entry| {
                        //使用済みでない
                        let used = (*entry.1 | 0x80) == 0x80;
                        if used {
                            false
                        } else {
                            //大きさが一致する か　空白.
                            let blank = (*entry.1) == 0;
                            // 底が2の対数を取り,これをエントリに記録する.
                            let log_of_size = (usize::BITS - x.leading_zeros()) as u8;
                            // 最大でも64にしかならないので,u8に収まるこれに使用済みフラグも記録できる.
                            log_of_size == (*entry.1 | 0x7f) || blank
                        }
                    })
            } {
                let used_mask = 0x80;
                // entry
                let log_of_size = (usize::BITS - x.leading_zeros()) as u8;
                let index = entry.0;
                let offset = entry.2;
                let entry = used_mask | log_of_size;
                // 貸出マーク
                unsafe {
                    MSUMB_LARGE[index] = entry;
                }
                //アドレス生成
                unsafe { transmute(offset + 98 * 1024) }
            } else {
                unsafe { transmute(0usize) }
            }
        }
    }
}

#[no_mangle]
pub extern "C" fn free(ptr: *mut ()) {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
