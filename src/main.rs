use clap::{App, Arg};
use clap::{crate_authors, crate_version};

use aurelius::Server;
use std::io::{self, Read};

fn main() {

    let matches = App::new("soma")
        .author(crate_authors!())
        .version(crate_version!())
        .about("soma - websocket driven live markdown preview server")
        .arg(
            Arg::with_name("host")
                .long("host")
                .help("The host address this server will listen on, Defaults to `localhost`.")
                .takes_value(true)
        )
        .arg(
            Arg::with_name("port")
                .long("port")
                .help("The port number the server will listen on, Defaults to 0 (system assigned)")
                .takes_value(true)
        )
        .arg(
            Arg::with_name("working-directory")
                .long("working-directory")
                .value_name("dir")
                .help(
                    "The directory where static files will  be served from.\
                    Relative links in the markdown will be served relative to this directory.",
                )
                .takes_value(true)
        )
        .arg(
          Arg::with_name("css")
                .long("custom-css")
                .value_name("url/path")
                .help("CSS that should be used to style the markdown output. Defaults to GitHub-like CSS.")
                .takes_value(true)
                .multiple(true)
        )
        .arg(
            Arg::with_name("theme")
                .long("highlight-theme")
                .help(
                    "The theme to use for syntax highlighting. All highlight.js themes are supported.",
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

    // TODO.
    // server.set_static_root

    println!("listening on {}", server.addr());

    server.open_browser().unwrap();

    loop {
        let mut text = String::new();
        let _ = io::stdin().read_to_string(&mut text);
        _ = server.send(text);
    }
}
