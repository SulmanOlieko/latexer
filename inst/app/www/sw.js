var CACHE_NAME = "latexer-cache-v1";
var urlsToCache = [
  "/",
  "index.html",
  "manifest.json",
  "sw.js",
  "https://unpkg.com/split.js/dist/split.min.js"
  "https://raw.githubusercontent.com/SulmanOlieko/latexer/master/latexer-sticker-1.png"
];
self.addEventListener("install", function(event) {
  event.waitUntil(
    caches.open(CACHE_NAME).then(function(cache) {
          console.log("Opened cache");
          return cache.addAll(urlsToCache);
    })
  );
});
self.addEventListener("fetch", function(event) {
  event.respondWith(
    caches.match(event.request).then(function(response) {
          return response || fetch(event.request);
    })
  );
});
