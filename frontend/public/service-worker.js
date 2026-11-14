// Prefer fresh assets on reload; retain a cache only for offline use.
const CACHE_NAME = 'v19';
const ASSETS_TO_CACHE = [
    '/',
    '/manifest.json',
    '/continugo.js',
    '/media/favicon.svg',
    '/media/style.css',
    '/media/board.css',
    '/media/rec-mono-csl-bold.woff2',
    '/media/rec-mono-csl-italic.woff2',
    '/media/rec-mono-csl-regular.woff2',
];

self.addEventListener('install', (event) => {
    event.waitUntil(
        caches.open(CACHE_NAME).then((cache) => {
            return cache.addAll(ASSETS_TO_CACHE.map((url) => new Request(url, { cache: 'reload' })));
        }).then(() => self.skipWaiting())
    );
});

self.addEventListener('fetch', (event) => {
    const url = new URL(event.request.url);
    if (event.request.method !== 'GET' || url.origin !== self.location.origin) return;
    const navigation = event.request.mode === 'navigate';
    if (!navigation && !ASSETS_TO_CACHE.includes(url.pathname)) return;

    event.respondWith((async () => {
        const cache = await caches.open(CACHE_NAME);
        const key = navigation ? '/' : event.request;
        let response;
        try {
            response = await fetch(event.request, { cache: 'no-cache' });
        } catch (error) {
            const cached = await cache.match(key);
            if (cached) return cached;
            throw error;
        }
        if (response.ok) event.waitUntil(cache.put(key, response.clone()));
        return response;
    })());
});

self.addEventListener('activate', (event) => {
    const cacheWhitelist = [CACHE_NAME];
    event.waitUntil(
        caches.keys().then((cacheNames) => {
            return Promise.all(
                cacheNames.map((cacheName) => {
                    if (!cacheWhitelist.includes(cacheName)) {
                        return caches.delete(cacheName);
                    }
                })
            );
        }).then(() => self.clients.claim())
    );
});
