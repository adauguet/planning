let margin = 15;

function generate(object) {
    console.log(object);
    var doc = new jsPDF()
    
    doc.setFontSize(12);
    doc.text(margin, 20, object.name);
    doc.text(margin, 25, object.month);
    
    for (i = 0; i < object.days.length; i++) {
        day(doc, i, object.days[i]);
    }

    doc.save('a4.pdf');
}

let height = 6;
let width = 210 - (2 * margin);

function day(doc, i, day) {
    doc.rect(margin, 50 + (height * i), width, height);
    doc.setFontSize(10);
    doc.text(day.date, margin + 30, 50 + (height * i) + 4, { align : "right" });
}