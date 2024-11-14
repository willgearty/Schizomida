// modified from https://github.com/glin/reactable/issues/385#issuecomment-2270009687
function downloadExcelFromTSV(tsv, filename, empty = false) {
  var wb = new ExcelJS.Workbook();
  var ws = wb.addWorksheet('Sheet1');

  // Parse the TSV to an array of objects
  var rows = Papa.parse(tsv, { header: true, delimiter: '\\t' }).data;
  var headers = Object.keys(rows[0] || {});

  // Create headers
  ws.columns = headers.map(header => ({
    header: header,
    key: header,
    style: { font: { name: 'Arial', size: 12, bold: true } }
  }));

  if (!empty) {
    // Add data
    rows.forEach(row => {
      ws.addRow(row);
    });
  
    // Format data cells
    ws.eachRow((row, rowNumber) => {
      row.eachCell((cell, colNumber) => {
        if (rowNumber > 1) { // Apply format only to data rows
          cell.style = { font: { name: 'Arial', size: 12 }, alignment: { horizontal: 'left' } };
        }
      });
    });
  }

  wb.xlsx.writeBuffer().then(function(buffer) {
    var blob = new Blob([buffer], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' });
    var url = window.URL.createObjectURL(blob);
    var a = document.createElement('a');
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    window.URL.revokeObjectURL(url);
  });
}

// Custom message handler for Shiny to export to Excel
function exportExcel(tab_id, filename, empty = false) {
  var tmp = $('.rt-tr-header .rt-th').map(function() {
              return this.getAttribute('aria-label').replace('Sort ', '');
            }).toArray()
  var tsv = Reactable.getDataCSV(tab_id, {columnIds: tmp, sep: '\\t' });
  downloadExcelFromTSV(tsv, filename, empty);
}
