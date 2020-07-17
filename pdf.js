let margin = 15;

function generate(object) {
  console.log(object);
  
  var doc = new jsPDF();

  doc.setFontSize(12);
  doc.text(margin, 20, object.name);
  doc.text(margin, 27, object.month);

  doc.setFontSize(14);
  doc.text("Feuille de temps", 105, 30, { align: "center" });
  doc.setFontSize(12);

  for (i = 0; i < object.days.length; i++) {
    drawDay(doc, 40, i, object.days[i]);
  }

  drawLegend(doc, 235, codes);
  drawSignatureRect(doc, 235);

  doc.save("a4.pdf");
}

let documentWidth = 210;
let dayHeight = 6;
let dayWidth = documentWidth - 2 * margin;

function drawDay(doc, top, i, day) {
  let x = margin;
  let y = top + dayHeight * i;
  let width = dayWidth;
  let height = dayHeight;

  doc.setFillColor("#E1E1E1");

  if (day.is_weekend) {
    doc.rect(x, y, width, height, "DF");
  } else {
    doc.rect(x, y, width, height);
  }
  doc.setFontSize(10);
  doc.text(day.date, x + 1, y + 4.5);

  let dateColumnWidth = 40;
  doc.line(x + dateColumnWidth, y, x + dateColumnWidth, y + dayHeight);

  day.ranges.forEach((range) => {
    drawRange(
      doc,
      range,
      x + dateColumnWidth,
      y,
      dayWidth - dateColumnWidth,
      height
    );
  });
}

function drawRange(
  doc,
  range,
  planningX,
  planningY,
  planningWidth,
  planningHeight
) {
  let b = timeToFloat(range.begin);
  let e = timeToFloat(range.end);

  let margin = 0;

  let x = ((b - 7.5) / (19 - 7.5)) * planningWidth + planningX;
  let y = planningY + margin;
  let width = ((e - b) / (19 - 7.5)) * planningWidth;
  let height = planningHeight - 2 * margin;

  doc.rect(x, y, width, height);

  doc.setFontSize(9);
  doc.text(range.code, x + 1, y + 3);

  doc.setFontSize(5);
  doc.text(
    timeToString(range.begin) + " - " + timeToString(range.end),
    x + 1,
    y + height - 0.9
  );
}

function timeToFloat(time) {
  return time.hours + time.minutes / 60;
}

function timeToString(time) {
  return (
    time.hours.toString().padStart(2, "0") +
    ":" +
    time.minutes.toString().padStart(2, "0")
  );
}

function drawSignatureRect(doc, y) {
  let width = 70;
  let x = documentWidth - margin - width;
  doc.setFontSize(10);
  doc.text("Signature du collaborateur", x, y - 2);
  doc.rect(x, y, width, 30);
}

function drawLegend(doc, y, codes) {
  doc.text("Légende", margin, y - 2);
  doc.setFontSize(8);
  for (i = 0; i < codes.length; i++) {
    doc.text(codes[i].code, margin, y + i * 4 + 3);
    doc.text(codes[i].description, margin + 10, y + i * 4 + 3);
  }
}

let codes = [
  { code: "T", description: "Heures travaillées" },
  { code: "TT", description: "Heures télétravaillées" },
  { code: "HS", description: "Heures supplémentaires" },
  { code: "NT", description: "Non Travaillées (et de professionnalisation)" },
  { code: "RCR", description: "Repos Compensateur de Récupération" },
  { code: "CP", description: "Congés Payés" },
  { code: "AT", description: "Arrêt de Travail" },
  { code: "F", description: "Férié" },
  { code: "JS", description: "Journée de Solidarité" },
  { code: "AAP", description: "Arrêt Activité Partielle" },
  { code: "AGE", description: "Arrêt Garde Enfants" },
];
