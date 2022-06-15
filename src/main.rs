use clap::Parser;

use aurelius::Server;
use std::io::{self, Read};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args{
    #[clap(long, default_value = "localhost")]
    host: String,
    #[clap(long, default_value = "0")]
    port: String,
    #[clap(long, default_value = "github")]
    highlight_theme: String,
    #[clap(long)]
    working_directory: Option<String>,
    #[clap(long)]
    custom_css: Option<Vec<String>>,
}

fn main() {

    let args = Args::parse();

    let mut server = Server::bind(format!("{}:{}", args.host, args.port)).unwrap();

    server.set_highlight_theme(args.highlight_theme);

    if let Some(custom_css) = args.custom_css {
        println!("CSS: {}", custom_css.join("\n"));
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
