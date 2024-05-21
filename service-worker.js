const CACHE_NAME = 'v1';
const ASSETS_TO_CACHE = [
    '/',
    '/manifest.json',
    '/continugo.js',
    '/service-worker.js',
    '/media/favicon.svg',
    '/media/style.css',
    '/media/rec-mono-csl-bold.woff2',
    '/media/rec-mono-csl-italic.woff2',
    '/media/rec-mono-csl-regular.woff2',
];

self.addEventListener('install', (event) => {
    event.waitUntil(
        caches.open(CACHE_NAME).then((cache) => {
            return cache.addAll(ASSETS_TO_CACHE);
        })
    );
});

self.addEventListener('fetch', (event) => {
    event.respondWith(
        caches.match(event.request).then((response) => {
            return response || fetch(event.request);
        })
    );
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
        })
    );
});
