use clap::{Command, Arg};

use aurelius::Server;
use std::io::{self, Read};

struct Args {
    #[derive(Parser, Debug)]
    #[clap(author, version, about, long_about = None)]
}

fn main() {

    let matches = Command::new("soma")
        .about("soma - websocket driven live markdown preview server")
        .arg(
            Arg::new("host")
                .long("host")
                .help("The host address this server will listen on, Defaults to `localhost`.")
                .takes_value(true)
        )
        .arg(
            Arg::new("port")
                .long("port")
                .help("The port number the server will listen on, Defaults to 0 (system assigned)")
                .takes_value(true)
        )
        .arg(
            Arg::new("working-directory")
                .long("working-directory")
                .value_name("dir")
                .help(
                    "The directory where static files will  be served from. \
                    Relative links in the markdown will be served relative to this directory.",
                )
                .takes_value(true)
        )
        .arg(
          Arg::new("css")
                .long("custom-css")
                .value_name("url/path")
                .help("CSS that should be used to style the markdown output. \
                       Defaults to GitHub-like CSS.")
                .takes_value(true)
                .multiple_occurrences(true)
        .arg(
            Arg::new("theme")
                .long("highlight-theme")
                .help(
                    "The theme to use for syntax highlighting. \
                     All highlight.js themes are supported.",
                )
                .default_value("github"),
        )
        .get_matches();

    let mut server = Server::bind(format!(
        "{}:{}",
        matches.value_of("host").unwrap_or("localhost"),
        matches.value_of("port").unwrap_or("0")
    )).unwrap();

    if let Some(custom_css) = matches.values_of("css") {
        server.set_custom_css(custom_css.map(String::from).collect()).unwrap() ;
    }

    if let Some(highlight_theme) = matches.value_of("theme") {
        server.set_highlight_theme(highlight_theme.to_string());
    }

    if let Some(working_directory) = matches.value_of("working-directory") {
        server.set_static_root(working_directory);
    }

    println!("listening on {}", server.addr());

    server.open_browser().unwrap();

    loop {
        let mut text = String::new();
        let _ = io::stdin().read_to_string(&mut text);
        _ = server.send(text);
    }
}
