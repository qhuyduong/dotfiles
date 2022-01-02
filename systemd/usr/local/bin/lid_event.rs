fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len()!=3 {
        println!("Cmd example: lid_event /proc/acpi/button/lid/LID0/state /dev/input/event1");
        std::process::exit(1);
    }
    let state_path = &args[1];
    let event_path = &args[2];

    let mut last_state = is_open(state_path);
    loop {
        std::thread::sleep(std::time::Duration::new(1,0));
        let state = is_open(state_path);
        if last_state && !state {
            println!("Generate lid-close event: {:?}", std::process::Command::new("evemu-event")
                     .args(&["--sync", event_path, "--type", "EV_SW", "--code", "SW_LID", "--value", "1"])
                     .output()
                     .expect("failed executing evemu-event"));
        }
        last_state = state;
    }
}

fn is_open(filename: &str) -> bool {
    let contents = std::fs::read_to_string(filename)
        .expect("failed reading the state file");
    return contents.find("open").is_some();
}
