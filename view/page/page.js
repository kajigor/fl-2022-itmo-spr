
function move_box(e) {
    const id = parseInt(e.target.getAttribute('data-indx'));
    const len = parseInt(e.target.getAttribute('data-size'));
    const final = parseInt(e.target.getAttribute('max'));
    const points = [0, 0, -1];
    const val = parseInt(e.target.value);
    let i = 0;

    while(i < val - 1) {

        if (i == final - 2) {
            ++i;
            continue;
        }

        if (points[2] <= 0) {
            ++points[1];
            points[2] = points[0] - 1;
        } else {
            --points[2];
        }

        if (points[1] === len - points[0]) {
            ++points[0];
            points[1] = 0;
            points[2] = points[0] - 1;
        }

        ++i;
    }

    for (const cell of document.getElementsByClassName(`ce_${id}`)) {
        cell.classList.remove('yellow');
        cell.classList.remove('red');
        cell.classList.remove('gray');
        for (const c of cell.children) {
            c.classList.add('hidden-text');
        }
    }

    for (const line of document.getElementsByClassName('dict-block-line-ch')) {
        line.children[0].classList.remove('blue');
        line.children[1].classList.remove('almost-blue');
    }

    if (val > 0) {

        let maincell = document.getElementById(`r_${id}_cell_${points[0]}_${points[1]}`);

        if (final !== val) {
            maincell.classList.add('yellow');
        } else {
            maincell.classList.add('gray');
        }

        for (let j = 0; j < points[0] * len + points[1]; ++j) {
            const y = Math.floor(j / len);
            const x = Math.floor(j % len);

            const cell = document.getElementById(`r_${id}_cell_${y}_${x}`);
            if (cell) {
                for (const c of cell.children) {
                    c.classList.remove('hidden-text');
                }
            }
        }

        for(const c of maincell.children) {
            if (c.dataset.item <= points[0] - 1 - points[2]) {                
                if ((c.dataset.item == points[0] - 1 - points[2]) || points[2] == -1) {

                    const rule = document.getElementById(`dkey_${id}_${c.innerHTML}`);

                    rule.children[0].classList.add('blue');
                    rule.children[1].classList.add('almost-blue');
                }               

                c.classList.remove('hidden-text')
            }
        }

        if (val === final) {
            return;
        }

        if (points[2] >= 0) {

            let c1_x = points[1];
            let c1_y = points[0] - 1 - points[2];

            let c1 = document.getElementById(`r_${id}_cell_${c1_y}_${c1_x}`);
            c1.classList.add('red');

            let c2_x = points[1] + (points[0] - points[2]);
            let c2_y = points[2];

            let c2 = document.getElementById(`r_${id}_cell_${c2_y}_${c2_x}`);
            c2.classList.add('red');
        
        }
    }
}

const sliders = document.getElementsByClassName('slider');
for (const slider of sliders) {
    slider.oninput = move_box;
}

const reseters = document.getElementsByClassName('reset-btn');
for (const reseter of reseters) {
    reseter.onclick = function(e) {
        const slider = document.getElementById(`sl_${e.target.dataset.indx}`);
        slider.value = slider.getAttribute('min');
        move_box({ target : slider});
    }
}

const finilisers = document.getElementsByClassName('final-btn');
for (const finiliser of finilisers) {
    finiliser.onclick = function(e) {
        const slider = document.getElementById(`sl_${e.target.dataset.indx}`);
        slider.value = slider.getAttribute('max');
        move_box({ target : slider});
    }
}

const prevs = document.getElementsByClassName('prev-btn');
for (const prev of prevs) {
    prev.onclick = function(e) {
        const slider = document.getElementById(`sl_${e.target.dataset.indx}`);
        slider.value = parseInt(slider.value) - 1;
        move_box({ target : slider});
    }
}

const nexts = document.getElementsByClassName('next-btn');
for (const next of nexts) {
    next.onclick = function(e) {
        const slider = document.getElementById(`sl_${e.target.dataset.indx}`);
        slider.value = parseInt(slider.value) + 1;
        move_box({ target : slider});
    }
}

for (const finiliser of finilisers) {
    finiliser.click();
}