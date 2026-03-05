const CACHE_NAME = "tennis-tracker-v1";

// Relative URLs are resolved against the SW's own location at install time,
// so these work correctly whether the app is at / or /tennis-tracker/.
const PRECACHE_URLS = [
  "./",
  "./index.html",
  "./build/main.js",
  "./manifest.json",
  "./icons/icon-192.png",
  "./icons/icon-512.png",
];

// ---------------------------------------------------------------------------
// Install — pre-cache the app shell
// ---------------------------------------------------------------------------
self.addEventListener("install", (event) => {
  event.waitUntil(
    caches
      .open(CACHE_NAME)
      .then((cache) => cache.addAll(PRECACHE_URLS))
      .then(() => self.skipWaiting()),
  );
});

// ---------------------------------------------------------------------------
// Activate — delete stale caches from previous versions
// ---------------------------------------------------------------------------
self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches
      .keys()
      .then((cacheNames) =>
        Promise.all(
          cacheNames
            .filter((name) => name !== CACHE_NAME)
            .map((name) => caches.delete(name)),
        ),
      )
      .then(() => self.clients.claim()),
  );
});

// ---------------------------------------------------------------------------
// Fetch — cache-first for local assets, network-first for CDN resources
// ---------------------------------------------------------------------------
self.addEventListener("fetch", (event) => {
  if (event.request.method !== "GET") return;

  const url = new URL(event.request.url);
  const isLocal = url.origin === self.location.origin;

  if (isLocal) {
    event.respondWith(
      caches.match(event.request).then((cachedResponse) => {
        if (cachedResponse) return cachedResponse;

        return fetch(event.request)
          .then((networkResponse) => {
            if (networkResponse && networkResponse.status === 200) {
              const clone = networkResponse.clone();
              caches
                .open(CACHE_NAME)
                .then((cache) => cache.put(event.request, clone));
            }
            return networkResponse;
          })
          .catch(() => {
            if (event.request.destination === "document") {
              // Resolve the shell URL relative to the SW scope
              return caches.match(
                new URL("./index.html", self.registration.scope).href,
              );
            }
          });
      }),
    );
  } else {
    // Network-first for external resources (Tailwind CDN) with cache fallback
    event.respondWith(
      fetch(event.request)
        .then((networkResponse) => {
          if (networkResponse && networkResponse.status === 200) {
            const clone = networkResponse.clone();
            caches
              .open(CACHE_NAME)
              .then((cache) => cache.put(event.request, clone));
          }
          return networkResponse;
        })
        .catch(() => caches.match(event.request)),
    );
  }
});
