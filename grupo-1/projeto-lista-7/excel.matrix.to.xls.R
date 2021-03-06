#' 
#' Exporta os cenarios para uma planilha xls
#' 

require("XLConnect")

excel.matrix.to.xls <- excel_matrix_to_xls <- excelMatrixToXls <- 
  function(matrizCenarios, nomeArquivoImagemGrafo, caminhosGrafo, nomeArquivo){
  
  #creating an Excel workbook.
  workbook <- loadWorkbook(nomeArquivo, create = TRUE)
  
  # matrizCenarios sheet
  createSheet(workbook, name = "cenarios")
  # graph sheet
  createSheet(workbook, name = "grafo")
  # all_simple_paths sheet
  createSheet(workbook, name = "caminhos-grafo")

  # writing the caminhosGrafo into caminhos-grafo sheet 
  writeWorksheet(workbook, caminhosGrafo, sheet = "caminhos-grafo", 
                 startRow = 1, startCol = 1)
  
  # writing the matrizCenarios into cenarios sheet 
  writeWorksheet(workbook, matrizCenarios, sheet = "cenarios", 
                 startRow = 1, startCol = 1)
  
  # ------------------ graph image ------------------------------------------#
  # writing the graph-image into grafo sheet
  # Create a named region called 'grafo' referring to the sheet 'grafo'
  createName(workbook, name = "grafo", formula = "grafo!$B$2")
  
  # Write image to the named region created above
  addImage(workbook, filename = nomeArquivoImagemGrafo, name = "grafo",
           originalSize = TRUE)
  # -------------------------------------------------------------------------#    

  # writeWorksheetToFile(nomeArquivo, data = matrizCenarios, sheet = "cenarios", 
  #                      startRow = 1, startCol = 1)

  #saving a workbook to an Excel file :
  saveWorkbook(workbook)
  
}