// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use tauri::WindowEvent;

fn main() {
    tauri::Builder::default()
        .on_window_event(|event| match event.event() {
            WindowEvent::FileDrop(drop) => { dbg!(drop); },
            _ => ()
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
