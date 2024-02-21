use std::net::{TcpListener, TcpStream};

pub fn run() {
    let listener = TcpListener::bind("127.0.0.1:7878").unwrap();

    for conn in listener.incoming() {
        let conn = conn.unwrap();
        handle_conn(conn);
    }
}

fn handle_conn(conn : TcpStream) {
    todo!()
}
