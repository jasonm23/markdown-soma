use clap::Parser;

use aurelius::Server;
use std::io::{self, Read};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args{
    #[clap(short, long, default_value = "localhost")]
    host: String,
    #[clap(short, long, default_value = "0")]
    port: String,
    #[clap(short, long, default_value = "github")]
    highlight_theme: String,
    #[clap(short, long)]
    working_directory: Option<String>,
    #[clap(short, long)]
    custom_css: Option<Vec<String>>,
}

fn main() {

    // let matches = Command::new("soma")
        // .server(
        //     Arg::new("host")
        //         .long("host")
        //         .help("The host address this server will listen on, Defaults to `localhost`.")
        //         .takes_value(true)
        // )
        // .arg(
        //     Arg::new("port")
        //         .long("port")
        //         .help("The port number the server will listen on, Defaults to 0 (system assigned)")
        //         .takes_value(true)
        // )
        // .arg(
        //     Arg::new("working-directory")
        //         .long("working-directory")
        //         .value_name("dir")
        //         .help(
        //             "The directory where static files will  be served from. \
        //             Relative links in the markdown will be served relative to this directory."
        //         )
        //         .takes_value(true)
        // )
        // .arg(
        //   Arg::new("css")
        //         .long("custom-css")
        //         .value_name("url/path")
        //         .help("CSS that should be used to style the markdown output. \
        //                Defaults to GitHub-like CSS.")
        //         .takes_value(true)
        //         .multiple_occurrences(true)
        // .arg(
        //     Arg::new("theme")
        //         .long("highlight-theme")
        //         .help(
        //             "The theme to use for syntax highlighting. \
        //              All highlight.js themes are supported.",
        //         )
        //         .default_value("github"),
        // )
        // .get_matches();
        //

    let args = Args::parse();

    let mut server = Server::bind(format!("{}:{}", args.host, args.port)).unwrap();

    server.set_highlight_theme(args.highlight_theme);

    if let Some(custom_css) = args.custom_css {
        _ = server.set_custom_css(custom_css);
    }

    if let Some(working_directory) = args.working_directory {
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
