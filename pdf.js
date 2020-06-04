let margin = 15;

function generate(object) {
    var doc = new jsPDF()
    
    doc.setFontSize(12);
    doc.text(margin, 20, object.name);
    doc.text(margin, 27, object.month);
    
    for (i = 0; i < object.days.length; i++) {
        drawDay(doc, i, object.days[i]);
    }

    doc.save('a4.pdf');
}

let documentWidth = 210;
let dayHeight = 6;
let dayWidth = documentWidth - (2 * margin);

function drawDay(doc, i, day) {

    let x = margin;
    let y = 50 + (dayHeight * i);
    let width = dayWidth;
    let height = dayHeight;

    doc.rect(x, y, width, height);
    doc.setFontSize(10);
    doc.text(day.date, x+1, y+4.5);

    let dateColumnWidth = 40;
    doc.line(x+dateColumnWidth, y, x+dateColumnWidth, y + dayHeight);

    if (day.kind === "default") {
        day.ranges.forEach(range => {
            drawRange(doc, range, x+dateColumnWidth, y, dayWidth - dateColumnWidth, height);
        });
    } else {
        console.log(day.kind);
    }
}

function drawRange(doc, range, planningX, planningY, planningWidth, planningHeight) {

    let b = timeToFloat(range.begin);
    let e = timeToFloat(range.end);

    let margin = 1;

    let x = ((b - 7.5) / (19 - 7.5)) * planningWidth + planningX;
    let y = planningY + margin;
    let width = ((e - b) / (19 - 7.5)) * planningWidth;
    let height = planningHeight - 2 * margin;
    
    doc.rect(x, y, width, height);

    doc.setFontSize(9);
    doc.text(range.code, x + 1, y + height - 0.9);
}

function timeToFloat(time) {
    return time.hours + time.minutes / 60;
}