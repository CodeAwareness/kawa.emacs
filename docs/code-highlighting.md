# Kawa Code Emacs Highlighting Implementation

## Overview

This document describes the highlighting implementation for the Kawa Code Emacs extension, which mirrors the functionality of the VSCode version.

## How It Works

### 1. JSON Data Structure
The local service sends JSON data with this structure:
```json
{
  "flow": "res",
  "domain": "code", 
  "action": "active-path",
  "data": {
    "hl": [0,4,7,25,28]
  }
}
```

The `hl` field contains line numbers (0-based) that need highlighting.

### 2. Data Processing
- `kawacode--handle-repo-active-path-response` extracts the `hl` data
- `kawacode--convert-hl-to-highlights` converts 0-based line numbers to 1-based (Emacs convention)
- `kawacode--apply-highlights-from-data` applies the highlights to the buffer

### 3. Highlighting System
- Uses Emacs overlays for temporary highlighting
- Theme-aware colors (automatically detects light/dark themes)
- Configurable intensity and timing
- Supports different highlight types: modified, conflict, peer, overlap
- **NEW**: Proper empty line highlighting support using hl-line technique
- **NEW**: Optional hl-line mode integration for better empty line handling
- **NEW**: JSON `hl` data automatically uses improved empty line highlighting

## Empty Line Highlighting Fix

### Problem
Previously, empty lines could not be highlighted because `(line-end-position line-number)` returns the same position as `(line-beginning-position line-number)` for empty lines, creating overlays with zero width.

### Solution
The highlighting system now uses the same technique as `hl-line.el`:
- Instead of `(line-end-position line-number)`, uses `(line-beginning-position (1+ line-number))`
- This ensures empty lines get proper overlay span from the start of the line to the start of the next line
- Works for both regular overlays and full-width overlays
- **Automatically applied to all JSON `hl` data from the local service**

## Configuration Options

### Highlighting Behavior
- `kawacode-highlight-intensity`: Highlight intensity (0.0 to 1.0)
- `kawacode-highlight-refresh-delay`: Delay before refreshing highlights
- `kawacode-highlight-persistent`: Persist highlights across buffer switches
- `kawacode-full-width-highlights`: Use full-width highlights

### Colors (Theme-Aware)
- `kawacode-change-color-light/dark`: Colors for changed lines
- `kawacode-peer-color-light/dark`: Colors for peer code
- `kawacode-merge-color-light/dark`: Colors for merged code

## Testing Commands

### Basic Testing
- `C-c C-a L`: Test highlighting with sample data (lines 1, 5, 10, 15)
- `C-c C-a i`: Debug current highlights in buffer
- `C-c C-a j`: Test JSON parsing with sample data
- **NEW**: `C-c C-a E`: Test empty line highlighting specifically
- **NEW**: `C-c C-a J`: Test JSON hl data flow (simulates actual local service data)

### Highlight Management
- `C-c C-a h`: Clear highlights in current buffer
- `C-c C-a H`: Clear all highlights
- `C-c C-a R`: Refresh highlights in current buffer

## Implementation Details

### Key Functions
1. **`kawacode--init-highlight-faces`**: Initialize theme-aware faces
2. **`kawacode--convert-agg-to-highlights`**: Convert JSON data to highlight format
3. **`kawacode--add-highlight`**: Add single highlight to buffer
4. **`kawacode--clear-buffer-highlights`**: Clear highlights from buffer
5. **`kawacode--apply-highlights-from-data`**: Apply multiple highlights
6. **NEW**: `kawacode--init-hl-line-faces`**: Initialize hl-line faces
7. **NEW**: `kawacode--add-hl-line-highlight`**: Add hl-line highlight
8. **NEW**: `kawacode--apply-hl-line-highlights-from-data`**: Apply hl-line highlights

### Data Flow
1. Local service sends JSON with `agg` data
2. Emacs parses JSON and extracts line numbers
3. Line numbers are converted to 1-based indexing
4. Overlays are created with appropriate faces (using hl-line technique for empty lines)
5. Highlights are applied to the active buffer

## Troubleshooting

### If highlights don't appear:
1. Check if the buffer is active: `C-c C-a i`
2. Test JSON parsing: `C-c C-a j`
3. Test highlighting system: `C-c C-a L`
4. Test empty line highlighting: `C-c C-a E`
5. Check logs: `C-c C-a l`

### If colors don't look right:
1. Check current theme: `(frame-parameter nil 'background-mode)`
2. Adjust color settings in `kawacode-config.el`
3. Restart Kawa Code mode

### If empty lines still don't highlight:
1. Ensure you're using the latest version with the hl-line technique
3. Test with `C-c C-a E` to verify empty line highlighting works

## Comparison with VSCode

| Feature | VSCode | Emacs |
|---------|--------|-------|
| Data Source | `project.hl` array | `data.hl` array |
| Line Numbers | 1-based | 0-based → 1-based conversion |
| Highlighting | Decorations | Overlays (with hl-line technique) |
| Empty Lines | Supported | **NEW**: Supported |
| Theme Support | Built-in | Manual detection |
| Configuration | Settings | Custom variables |
| HL-Line Integration | N/A | **NEW**: Optional |

## Future Improvements

1. **Font-lock integration**: Use font-lock for more persistent highlighting
2. **Gutter indicators**: Add symbols in the fringe/margin
3. **Animation support**: Flash highlights like VSCode
4. **Better theme integration**: Use theme colors directly
5. **Performance optimization**: Batch overlay operations
6. **Enhanced hl-line integration**: More seamless integration with existing hl-line modes
