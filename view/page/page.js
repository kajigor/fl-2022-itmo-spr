
const sliders = document.getElementsByClassName('slider');
for (const slider of sliders) {
    slider.oninput = function(e) {
        const points = [[0,0],[]];
        const val = e.target.value;
        console.log(val);
    }
}
