use std::{
    io::{BufRead, BufReader, Write},
    net::{Shutdown, TcpListener, TcpStream},
};

pub fn run() {
    let listener = TcpListener::bind("127.0.0.1:1337").unwrap();

    for conn in listener.incoming() {
        match conn {
            Ok(stream) => handle_conn(stream),
            Err(x) => println!("{}", x),
        }
    }
}

fn handle_conn(stream: TcpStream) {
    println!("handle start");
    let mut stream = stream;
    let buf_reader = BufReader::new(&mut stream);
    let data: Vec<_> = buf_reader
        .lines()
        .map(|result| result.unwrap())
        .take_while(|line| !line.is_empty())
        .collect();

    stream.shutdown(Shutdown::Read).unwrap();

    let data = data
        .into_iter()
        .reduce(|acc, x| acc + "\n" + x.as_str())
        .unwrap()
        + "\n";
    println!("{:?}", data);
    stream.write(data.into_bytes().as_slice()).unwrap();
    stream.flush().unwrap();
    stream.shutdown(Shutdown::Write).unwrap();
}
