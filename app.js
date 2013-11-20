
var qs = document.querySelector.bind(document);
var qsa = document.querySelectorAll.bind(document);

qs('.compile').addEventListener('click', function() {
    var code = qs('.code textarea').value;
    qs('.output').innerHTML = compile(code);
});

function ajaxGet(url, success) {
    var ajax = new XMLHttpRequest();

    ajax.onreadystatechange = function() {
        if(ajax.readyState == 4 && ajax.status == 200) {
            success(ajax.responseText);
        }
    };

    ajax.open('GET', url, true);
    ajax.send();
}

function compile(code) {
    qs('.error').classList.remove('show');

    try {
        var src = LJC.compile(code, {
            filename: 'console',
            'exported-funcs': 'main,render,getPixelsLength,getWidth,getHeight',
            'module-name': 'minecraft',
            asmjs: true
        });
    }
    catch(e) {
        var err = qs('.error');
        err.innerHTML = e.toString();
        err.classList.add('show');
        return '';
    }

    src = src.replace('+3.14159265359', '3.14159265359');
    eval(src);
    reinit();

    return src;
}

document.addEventListener('keydown', function(e) {
    if(e.keyCode == 83) {
        qs('.editor').classList.toggle('hide');
        qs('.instructions').classList.toggle('hide');
    }
});

ajaxGet('render.ljs', function(text) {
    qs('.code textarea').value = text;
});
