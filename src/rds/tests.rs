use std::io::{BufReader, BufWriter, Cursor, Write};

use self::rds_reader::RDSReader;
use self::rds_writer::RDSWriter;

use super::*;

macro_rules! test_data {
    ( $( $x:expr ),* $(,)?) => {
        let data : Vec<u8> = vec![$($x,)*];

        let data = Cursor::new(data);
        let mut reader = BufReader::new(data);
        let RDSResult {header, data : res} = reader.read_rds().unwrap();
        insta::assert_debug_snapshot!(res);

        let outdata: Vec<u8> = vec![];
        let mut writer = BufWriter::new(outdata);
        writer.write_rds(header, res).unwrap();
        writer.flush().unwrap();

        assert_eq!(writer.get_ref(), reader.get_ref().get_ref());
    }
}

macro_rules! test {
    ( $name:ident, $( $x:expr ),* $(,)?) => {
        mod $name {
            use super::*;
            #[test]
            fn reader() {
                let data : Vec<u8> = vec![$($x,)*];

                let data = Cursor::new(data);
                let mut reader = BufReader::new(data);
                let RDSResult {header: _, data : res} = reader.read_rds().unwrap();
                insta::assert_debug_snapshot!(res);
            }

            #[test]
            fn writer() {
                let indata: Vec<u8> = vec![$($x,)*];

                let data = Cursor::new(indata);
                let mut reader = BufReader::new(data);
                let input = reader.read_rds().unwrap();

                println!("{:?}", input.data);

                let outdata: Vec<u8> = vec![];
                let mut writer = BufWriter::new(outdata);
                writer.write_rds(input.header, input.data).unwrap();
                writer.flush().unwrap();

                assert_eq!(writer.get_ref(), reader.get_ref().get_ref());
            }
        }
    }
}

#[test]
fn test_header() {
    let data: Vec<u8> = vec![b'X', b'\n', 0, 0, 0, 2, 1, 1, 1, 1, 2, 2, 2, 2];
    let data = Cursor::new(data);
    let mut reader = BufReader::new(data);
    reader.read_header().unwrap();
}

#[test]
fn test_real() {
    // 1
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00,
        0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];

    // c(1, 2)
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00,
        0x02, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00,
    ];
}

#[test]
fn test_intsxp() {
    // as.integer(c(1, 2))
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02,
    ];
}

test![
    intsxp, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
    0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02,
];

#[test]
fn test_vecsxp() {
    // list(1, 2)
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x13, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00,
    ];
}

#[test]
fn test_list_tag_01() {
    // list(a=1)
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x02, 0x13, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00,
        0x00, 0x00, 0x05, 0x6e, 0x61, 0x6d, 0x65, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x61, 0x00, 0x00, 0x00, 0xfe,
    ];
}

#[test]
fn test_list_tag_02() {
    // list(a=1, b=2, 1)
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
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
}

#[test]
fn test_langsxp_01() {
    // substitute(f(x))
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x66, 0x00, 0x00, 0x00, 0x02, 0x00,
        0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x78, 0x00, 0x00, 0x00,
        0xfe,
    ];
}

#[test]
fn test_langsxp_02() {
    // substitute(x + 1)
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x2b, 0x00, 0x00, 0x00, 0x02, 0x00,
        0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x78, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0xfe,
    ];
}

#[test]
fn test_langsxp_03() {
    // substitute(hello(1)(2))
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00,
        0x06, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x68, 0x65,
        0x6c, 0x6c, 0x6f, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01,
        0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0xfe,
    ];
}

#[test]
fn test_langsxp_04() {
    // substitute(x + y)
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x2b, 0x00, 0x00, 0x00, 0x02, 0x00,
        0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x78, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x79, 0x00,
        0x00, 0x00, 0xfe,
    ];
}

#[test]
fn test_closxp01() {
    // function(x) x
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00, 0x00, 0x00,
        0xfd, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00,
        0x00, 0x01, 0x78, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x01, 0xff,
    ];
}

#[test]
fn test_closxp02() {
    // function(x, y) x + y
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00, 0x00, 0x00,
        0xfd, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00,
        0x00, 0x01, 0x78, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01,
        0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x79, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00,
        0x00, 0xfe, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00,
        0x00, 0x00, 0x01, 0x2b, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x01, 0xff, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x02, 0xff, 0x00, 0x00, 0x00, 0xfe
    ];
}

#[test]
fn test_closxp03() {
    // function() NULL ## but with attributes
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
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
}

#[test]
fn test_closxp04() {
    // function() NULL
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
        0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00, 0x00, 0x00,
        0xfd, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe,
    ];
}

#[test]
fn test_bcreader_01() {
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
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
}

#[test]
fn test_bcreader_02() {
    test_data![
        0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00, 0x00,
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
}
