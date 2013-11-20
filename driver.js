
var ctx;
var pixels;
var stats;

var renderer;
var w = 800;
var h = 600;
var render = true;
var _reinit = false;

var random = Math.random;
var now = Date.now;
var sin = Math.sin;
var cos = Math.cos;
var sqrt = Math.sqrt;
var PI = Math.PI;

function init() {
    if(minecraft.main(w, h) !== 0) {
        throw new Error("error initializing (probably too large screen)");
    }

    var canvas = document.getElementById('game');
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    renderer = new GLRenderer(canvas);

    if(renderer.unsupported) {
        alert('WebGL is required and not supported on your system.');
    }

    requestAnimationFrame(clock);
};

function reinit() {
    _reinit = true;
}

function clock() {
    if(_reinit) {
        minecraft.main(w, h);
        _reinit = false;
    }

    var ptr = minecraft.render();
    var length = minecraft.getPixelsLength();
    w = minecraft.getWidth();
    h = minecraft.getHeight();
    var pixels = window.U1.subarray(ptr, ptr + length);

    if(render) {
        if(renderer.isReady) {
            renderer.loadTexture(pixels, w, h);
            renderer.render();
        }
    }

    requestAnimationFrame(clock);
};

var random = Math.random;
var now = Date.now;

document.addEventListener('DOMContentLoaded', init);
