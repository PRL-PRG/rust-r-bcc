use std::io::{BufReader, BufWriter, Cursor, Read, Write};

use std::cell::UnsafeCell;

use self::rds_reader::RDSReader;
use self::rds_writer::RDSWriter;
use bumpalo::Bump;
use crate::sexp::sexp_alloc::Alloc;

use super::*;

macro_rules! test {
    ( $name:ident, $( $x:expr ),* $(,)?) => {
        mod $name {
            use super::*;
            #[test]
            fn reader() {
                let data : Vec<u8> = vec![$($x,)*];
                let arena = Bump::new();
                let arena = Alloc::new(&arena);

                let data = Cursor::new(data);
                let reader = BufReader::new(data);
                let reader = RDSReader::new(UnsafeCell::new(reader), &arena);
                let RDSResult {header: _, data : res} = reader.read_rds().unwrap();
                insta::assert_debug_snapshot!(res);
            }

            #[test]
            fn writer() {
                let indata: Vec<u8> = vec![$($x,)*];

                let arena = Bump::new();
                let arena = Alloc::new(&arena);

                let data = Cursor::new(indata);
                let reader_b = BufReader::new(data);
                let reader = RDSReader::new(UnsafeCell::new(reader_b), &arena);
                let input = reader.read_rds().unwrap();

                println!("{}", input.data);

                let outdata: Vec<u8> = vec![];
                let mut writer = BufWriter::new(outdata);
                writer.write_rds(input.header, input.data, &arena).unwrap();
                writer.flush().unwrap();

                assert_eq!(writer.get_ref(), &vec![$($x,)*]/*reader_b.get_ref().get_ref()*/);
            }
        }
    }
}

#[test]
fn test_header() {
    /*let data: Vec<u8> = vec![b'X', b'\n', 0, 0, 0, 2, 1, 1, 1, 1, 2, 2, 2, 2];
    let data = Cursor::new(data);
    let mut reader = BufReader::new(data);
    reader.read_header().unwrap();*/
}

// c(1, 2)
test![
    realsxp_02, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00,
    0x02, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00,
];

// 1
test![
    realsxp_01, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00,
    0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
];

test![
    intsxp, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
    0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02,
];

test![
    vecsxp, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x13, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00,
];

// list(a=1)
test![
    list_tag_01, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x02, 0x13, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00,
    0x00, 0x00, 0x05, 0x6e, 0x61, 0x6d, 0x65, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x61, 0x00, 0x00, 0x00, 0xfe,
];

// list(a=1, b=2, 1)
test![
    list_tag_02, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x02, 0x13, 0x00, 0x00, 0x00,
    0x03, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00,
    0x09, 0x00, 0x00, 0x00, 0x05, 0x6e, 0x61, 0x6d, 0x65, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00,
    0x00, 0x00, 0x03, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x61, 0x00, 0x04, 0x00,
    0x09, 0x00, 0x00, 0x00, 0x01, 0x62, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xfe,
];

// substitute(f(x))
test![
    langsxp_01, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x66, 0x00, 0x00, 0x00, 0x02, 0x00,
    0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x78, 0x00, 0x00, 0x00,
    0xfe,
];

// substitute(x + 1)
test![
    langsxp_02, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x2b, 0x00, 0x00, 0x00, 0x02, 0x00,
    0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x78, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0xfe,
];

// substitute(hello(1)(2))
test![
    langsxp_03, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00,
    0x06, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x68, 0x65,
    0x6c, 0x6c, 0x6f, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01,
    0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0xfe,
];

// substitute(x + y)
test![
    langsxp_04, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x2b, 0x00, 0x00, 0x00, 0x02, 0x00,
    0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x78, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x79, 0x00,
    0x00, 0x00, 0xfe,
];

// function(x) x
test![
    closxp01, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00, 0x00, 0x00,
    0xfd, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00,
    0x00, 0x01, 0x78, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x01, 0xff,
];

// function(x, y) x + y
test![
    closxp02, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00, 0x00, 0x00,
    0xfd, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00,
    0x00, 0x01, 0x78, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01,
    0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x79, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00,
    0x00, 0xfe, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00,
    0x00, 0x00, 0x01, 0x2b, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x01, 0xff, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x02, 0xff, 0x00, 0x00, 0x00, 0xfe
];

// function() NULL ## but with attributes
test![
    closxp03, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x06, 0x03, 0x00, 0x00, 0x04,
    0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x06, 0x73, 0x72,
    0x63, 0x72, 0x65, 0x66, 0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x17, 0x00, 0x00,
    0x00, 0x09, 0x00, 0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00,
    0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x07,
    0x73, 0x72, 0x63, 0x66, 0x69, 0x6c, 0x65, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xf2, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00,
    0x09, 0x00, 0x00, 0x00, 0x05, 0x6c, 0x69, 0x6e, 0x65, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00,
    0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x39, 0x73, 0x61, 0x76, 0x65,
    0x52, 0x44, 0x53, 0x28, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x28, 0x29, 0x20,
    0x4e, 0x55, 0x4c, 0x4c, 0x2c, 0x20, 0x66, 0x69, 0x6c, 0x65, 0x3d, 0x22, 0x74, 0x6d, 0x70,
    0x2e, 0x64, 0x61, 0x74, 0x22, 0x2c, 0x20, 0x63, 0x6f, 0x6d, 0x70, 0x72, 0x65, 0x73, 0x73,
    0x3d, 0x46, 0x41, 0x4c, 0x53, 0x45, 0x29, 0x0a, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x08, 0x66, 0x69, 0x6c, 0x65, 0x6e, 0x61,
    0x6d, 0x65, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x04, 0x02,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x63, 0x6c, 0x61,
    0x73, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x02, 0x00, 0x04, 0x00, 0x09, 0x00,
    0x00, 0x00, 0x0b, 0x73, 0x72, 0x63, 0x66, 0x69, 0x6c, 0x65, 0x63, 0x6f, 0x70, 0x79, 0x00,
    0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x07, 0x73, 0x72, 0x63, 0x66, 0x69, 0x6c, 0x65, 0x00,
    0x00, 0x00, 0xfe, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x06, 0xff, 0x00, 0x00, 0x00, 0x10,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x06, 0x73, 0x72, 0x63,
    0x72, 0x65, 0x66, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfd,
    0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe,
];

// function() NULL
test![
    closxp04, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00, 0x00, 0x00,
    0xfd, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe,
];

test![
    bcreader_01, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x06, 0x03, 0x00, 0x00, 0x04,
    0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x06, 0x73, 0x72,
    0x63, 0x72, 0x65, 0x66, 0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x26, 0x00, 0x00,
    0x00, 0x18, 0x00, 0x00, 0x00, 0x26, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00,
    0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x07,
    0x73, 0x72, 0x63, 0x66, 0x69, 0x6c, 0x65, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xf2, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00,
    0x09, 0x00, 0x00, 0x00, 0x05, 0x6c, 0x69, 0x6e, 0x65, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00,
    0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x28, 0x66, 0x63, 0x20, 0x3c,
    0x2d, 0x20, 0x63, 0x6f, 0x6d, 0x70, 0x69, 0x6c, 0x65, 0x72, 0x3a, 0x3a, 0x63, 0x6d, 0x70,
    0x66, 0x75, 0x6e, 0x28, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x28, 0x29, 0x20,
    0x4e, 0x55, 0x4c, 0x4c, 0x29, 0x0a, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00,
    0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x08, 0x66, 0x69, 0x6c, 0x65, 0x6e, 0x61, 0x6d, 0x65,
    0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00,
    0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x63, 0x6c, 0x61, 0x73, 0x73,
    0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x02, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00,
    0x0b, 0x73, 0x72, 0x63, 0x66, 0x69, 0x6c, 0x65, 0x63, 0x6f, 0x70, 0x79, 0x00, 0x04, 0x00,
    0x09, 0x00, 0x00, 0x00, 0x07, 0x73, 0x72, 0x63, 0x66, 0x69, 0x6c, 0x65, 0x00, 0x00, 0x00,
    0xfe, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x06, 0xff, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00,
    0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x06, 0x73, 0x72, 0x63, 0x72, 0x65,
    0x66, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfd, 0x00, 0x00,
    0x00, 0xfe, 0x00, 0x00, 0x00, 0x15, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x0d, 0x00,
    0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x11, 0x00, 0x00, 0x00, 0x01,
    0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00,
    0x0d, 0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
    0x00, 0x18, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x26, 0x00, 0x00, 0x00, 0x18, 0x00,
    0x00, 0x00, 0x26, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x04, 0x02,
    0x00, 0x00, 0x02, 0xff, 0x00, 0x00, 0x03, 0xff, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x06,
    0xff, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00,
    0x00, 0x06, 0x73, 0x72, 0x63, 0x72, 0x65, 0x66, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00,
    0x0d, 0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00, 0x03, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x06, 0xff, 0x00,
    0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x10,
    0x65, 0x78, 0x70, 0x72, 0x65, 0x73, 0x73, 0x69, 0x6f, 0x6e, 0x73, 0x49, 0x6e, 0x64, 0x65,
    0x78, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x03, 0x0d, 0x00, 0x00,
    0x00, 0x03, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00,
    0x00, 0x04, 0x02, 0x00, 0x00, 0x06, 0xff, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01,
    0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x0c, 0x73, 0x72, 0x63, 0x72, 0x65, 0x66, 0x73,
    0x49, 0x6e, 0x64, 0x65, 0x78, 0x00, 0x00, 0x00, 0xfe
];

test![
    bcreader_02, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00, 0x00, 0x00,
    0xfd, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x15, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
    0x00, 0x0d, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x11, 0x00,
    0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe,
    0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00, 0x03, 0x80, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00,
    0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x63, 0x6c, 0x61, 0x73, 0x73,
    0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00,
    0x10, 0x65, 0x78, 0x70, 0x72, 0x65, 0x73, 0x73, 0x69, 0x6f, 0x6e, 0x73, 0x49, 0x6e, 0x64,
    0x65, 0x78, 0x00, 0x00, 0x00, 0xfe,
];

// compiler::cmpfun(function() NULL)
test![
    bcreader_03, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x03, 0x00, 0x03, 0x05, 
    0x00, 0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00, 
    0x00, 0x00, 0xfd, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 
    0x00, 0x00, 0x00, 0x01, 0x66, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00,
    0x00, 0x15, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x00, 0x06, 0x00,
    0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x26,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00,
    0x06, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0xff, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x01, 0xff, 0x00,
    0x00, 0x00, 0x0d, 0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00, 0x06, 0x80, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04,
    0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x63, 0x6c, 0x61, 0x73, 0x73, 0x00, 0x00, 0x00, 0x10,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x10, 0x65, 0x78, 0x70,
    0x72, 0x65, 0x73, 0x73, 0x69, 0x6f, 0x6e, 0x73, 0x49, 0x6e, 0x64, 0x65, 0x78, 0x00, 0x00,
    0x00, 0xfe, 
];


// compiler::cmpfun(function(x, y) x + y, options=list(optimize=0))
test![
    bcreader_04, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x03, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00, 0x00, 0x00,
    0xfd, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00,
    0x00, 0x01, 0x78, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01,
    0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x79, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00,
    0x00, 0xfe, 0x00, 0x00, 0x00, 0x15, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x0d, 0x00,
    0x00, 0x00, 0x0a, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0x01,
    0x00, 0x00, 0x00, 0x1d, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x1d, 0x00, 0x00, 0x00,
    0x03, 0x00, 0x00, 0x00, 0x26, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
    0x00, 0x05, 0x00, 0x00, 0x00, 0xf4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00,
    0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09,
    0x00, 0x00, 0x00, 0x01, 0x2b, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x01, 0xff, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0xfe, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x03, 0xff, 0x00, 0x00, 0x00, 0x15, 0x00, 0x00, 0x00,
    0x0d, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x14, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x01, 0x00,
    0x00, 0x01, 0xff, 0x00, 0x00, 0x00, 0xf3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0d,
    0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00, 0x04, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00,
    0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x63, 0x6c, 0x61, 0x73, 0x73,
    0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00,
    0x10, 0x65, 0x78, 0x70, 0x72, 0x65, 0x73, 0x73, 0x69, 0x6f, 0x6e, 0x73, 0x49, 0x6e, 0x64,
    0x65, 0x78, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x15, 0x00, 0x00, 0x00, 0x0d, 0x00,
    0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x02,
    0xff, 0x00, 0x00, 0x00, 0xf3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00,
    0x03, 0x0d, 0x00, 0x00, 0x00, 0x04, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00,
    0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x04, 0xff,
    0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00,
    0x10, 0x65, 0x78, 0x70, 0x72, 0x65, 0x73, 0x73, 0x69, 0x6f, 0x6e, 0x73, 0x49, 0x6e, 0x64,
    0x65, 0x78, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x03, 0x0d, 0x00,
    0x00, 0x00, 0x0a, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x04, 0x02, 0x00, 0x00, 0x04, 0xff, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00,
    0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x10, 0x65, 0x78, 0x70, 0x72, 0x65, 0x73, 0x73, 0x69,
    0x6f, 0x6e, 0x73, 0x49, 0x6e, 0x64, 0x65, 0x78, 0x00, 0x00, 0x00, 0xfe, 
];

// as.logical(c(0, 1))
test![
    lglsxp_01, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x03, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0a, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 
];

// empty hash environment
test![
    hashenv_01, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x03, 0x00, 0x03, 0x05, 0x00, 0x00,
    0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xfd, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x13, 0x00, 0x00,
    0x00, 0x1d, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00,
    0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe,
    0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00,
    0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00,
    0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00,
    0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe,
    0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00,
    0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00,
    0x00, 0xfe, 
];

macro_rules! testR {
    ( $name:ident, $code:expr) => {
        mod $name {
            use super::*;
            #[test]
            fn reader() {
                let path = format!("temp/{}.dat", stringify!($name));
                let path = path.as_str();
                let mut command = std::process::Command::new("./create_serdata.R")
                    .args(["-d", $code, path]).spawn().unwrap();
                assert!(command.wait().unwrap().success());

                let file = std::fs::File::open(path).unwrap();

                let arena = Bump::new();
                let arena = Alloc::new(&arena);
                let file = RDSReader::new(UnsafeCell::new(file), &arena);
                let RDSResult { header: _, data } = file.read_rds().unwrap();

                insta::assert_debug_snapshot!(data);
                std::fs::remove_file(path).unwrap();
            }

            #[test]
            fn writer() {
                let path = format!("temp/{}_writer.dat", stringify!($name));
                let path = path.as_str();
                let mut command = std::process::Command::new("./create_serdata.R")
                    .args(["-d", $code, path]).spawn().unwrap();
                assert!(command.wait().unwrap().success());
            
                let file = std::fs::File::open(path).unwrap();

                let arena = Bump::new();
                let arena = Alloc::new(&arena);
                let file = RDSReader::new(UnsafeCell::new(file), &arena);
                let RDSResult { header, data : input } = file.read_rds().unwrap();

                let mut input_vec = vec![];
                let mut file = std::fs::File::open(path).unwrap();
                file.read_to_end(&mut input_vec).unwrap();

                println!("{}", input);

                let outdata: Vec<u8> = vec![];
                let mut writer = BufWriter::new(outdata);
                writer.write_rds(header, input, &arena).unwrap();
                writer.flush().unwrap();

                assert_eq!(writer.get_ref(), &input_vec);
                std::fs::remove_file(path).unwrap();
            }
        }
    }
}

testR![intsxp_02, "as.integer(c(1, 2))"];
testR![intsxp_03, "as.integer(c(1, 2, 3))"];
testR![strsxp_01, "'a'"];
testR![strsxp_02, "c('a', 'ahoj')"];
testR![closxp05, "function(a, b = 0) {x <- c(a, 1); x[[b]];}"];
testR![bcsxp_01, "compiler::cmpfun(function(a, b = 0) {x <- c(a, 1); x[[b]];})"];
testR![closxp06, "function(x) x + 1"];
testR![cplsxp01, "1+2i"];
testR![cplsxp02, "c(1+2i, 10i)"];

testR![
    basefun_shadowed,
    "
    compiler::cmpfun(function(x) {
        list <- function(...) print(1);
        list(x);
    }, options=list(optimize=0))"
];

testR![
    basefun_shadowed_opt,
    "
    compiler::cmpfun(function(x) {
        list <- function(...) print(1);
        list(x);
    })"
];

testR![
    tmp,
    "
    compiler::cmpfun(function(x) {
        function(...) 1;
    }, options=list(optimize=0))"
];

testR![
    hashenv_02, "{e <- new.env(); e$a <- 1; e$b <- 2; e }"
];

testR![
    dotrow,
    "compiler::cmpfun(function(dim) .Internal(row(dim)))"
];

testR![
    two_calls,
    "
    function (x) {
        g(x);
        f(x);
    }"
];

testR![
    two_calls_cmp,
    "
    compiler::cmpfun(
    function (x) {
        g(x);
        f(x);
    })"
];
