// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

fn main() {
    tauri::Builder::default()
        .on_page_load(|window, _payload| {
            window.listen("tauri://file-drop-hover", |event| { dbg!(event); });
            window.listen("tauri://file-drop", |event| { dbg!(event); });
            window.listen("tauri://file-drop-cancelled", |event| { dbg!(event); });
            dbg!("Listening...");
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
