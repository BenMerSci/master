import camelot

tables = camelot.read_pdf("chesapeake.pdf", pages = "13,14", flavor = "stream", strip_text = " .\n", row_tole = 10, table_areas = ["74,680,522,420"], columns = ["100,132,172,210,241,269,301,336,370,407,446,485,529"])
tables