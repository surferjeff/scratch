<template>
  <div class="container">
    <h1>Drag and drop files into this window.</h1>

    <div>
      Javascript sees the drag and drop events.  They get displayed below.
      However, Rust sees no such events.  The dbg!() statements in main.rs never
      execute.
    </div>
    <div v-if="dropEvent" class="row">
      {{ dropEvent }}
    </div>

  </div>
</template>

<script setup lang="ts">
import { onMounted, onUnmounted, ref } from 'vue';
import * as tauriEvent from '@tauri-apps/api/event';

const dropEvent = ref<any>(null);
let tauriListeners: tauriEvent.UnlistenFn[] = [];

onMounted(() => {
  const push = (n: tauriEvent.UnlistenFn) => tauriListeners.push(n);
  tauriEvent.listen('tauri://file-drop-hover', event => dropEvent.value = event).then(push);
  tauriEvent.listen('tauri://file-drop', event => dropEvent.value = event).then(push);
  tauriEvent.listen('tauri://file-drop-cancelled', event => dropEvent.value = event).then(push);
});

onUnmounted(() => {
  for (let unlisten of tauriListeners) {
    unlisten();
  }
  tauriListeners = [];
});

</script>

