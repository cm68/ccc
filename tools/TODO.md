# wsld TODO

## Hi-Tech Linker Compatibility

- [x] Define __Lbss and __Hbss symbols
  - Implemented: linker detects these symbols and patches them with BSS bounds
- [x] Define __Ltext, __Htext, __Ldata, __Hdata symbols
  - Implemented: linker detects and patches all segment bound symbols
