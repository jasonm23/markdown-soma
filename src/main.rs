use clap::{App, Arg};
use clap::{crate_authors, crate_version};

use aurelius::Server;
use std::io::{self, Read};

fn main() {

    let matches = App::new("soma")
        .author(crate_authors!())
        .version(crate_version!())
        .about("soma - socket driven live markdown preview server")
        .arg(
            Arg::with_name("host")
                .long("host")
                .help("The host address this server will listen on, Defaults to `localhost`.")
        )
        .arg(
            Arg::with_name("port")
                .long("port")
                .help("The port number the server will listen on, Defaults to 0 (system assigned)")
        )
        .get_matches();

    let mut server = Server::bind(format!(
        "{}:{}",
        matches.value_of("host").unwrap_or("localhost"),
        matches.value_of("port").unwrap_or("0")
    )).unwrap();

    // TODO.
    // server.set_custom_css
    // server.set_highlight_theme
    // server.set_static_root

    println!("listening on {}", server.addr());

    server.open_browser().unwrap();

    loop {
        let mut text = String::new();
        let _ = io::stdin().read_to_string(&mut text);
        _ = server.send(text);
    }
}
