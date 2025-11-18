# Extract Stream Path Function - Step-by-Step Fix Approach

## Summary of Current Issues

The `extract_stream_path()` function has several critical issues preventing it from correctly extracting stream paths between two points on a network:

### 1. **Node vs. Edge Index Confusion** (CRITICAL)
- **Problem**: `find_shortest_path_network()` returns node indices, but `extract_path_segments()` treats them as streamline row indices
- **Impact**: Function selects wrong streamline segments, producing incorrect results
- **Solution**: Return actual row indices from the shortest path edges instead of node indices

### 2. **Missing Streamline Splitting Step** (CRITICAL)
- **Problem**: Streamlines are never split at the snapped point locations before creating the network
- **Impact**: Impossible to accurately identify where to clip the start/end segments; network nodes may not align with snap points
- **Solution**: Add a `split_streamlines_at_points()` function that splits streamlines at snapped coordinates before network creation

### 3. **Incomplete Segment Clipping Logic** (CRITICAL)
- **Problem**: Path segments aren't properly clipped at snapped point intersections
- **Impact**: Final path includes unwanted portions of streamlines before point 1 or after point 2
- **Solution**: Implement dedicated clipping functions that remove the unwanted portions while preserving attributes

### 4. **Attribute Loss in Result** (MEDIUM)
- **Problem**: Original `rid` and other attributes from the streamlines aren't preserved properly
- **Impact**: Test expects specific `rid` values to be present but they're missing
- **Solution**: Preserve attributes through all split/clip operations using `sf::st_drop_geometry()`

### 5. **Same-Line Special Case** (MEDIUM)
- **Problem**: When both points snap to the same streamline, function returns a direct line between original points instead of the actual snapped segment
- **Impact**: Wrong geometry for paths where both points are on the same stream
- **Solution**: Properly handle case where both points are on the same line by splitting into 3 parts (before pt1, pt1-to-pt2, after pt2)

---

## Step-by-Step Fix Approach

### **Phase 1: Add Streamline Splitting Functions** (Lines 149-347)

These are helper functions that must be implemented first because they're used early in the workflow:

#### Step 1A: `split_line_at_point(line, point_coords)` - Lines 209-257
**Purpose**: Split a single LINESTRING at one point location
**Key operations**:
1. Extract coordinates from line geometry
2. Find the closest line segment to the point (minimize distance to segment)
3. Interpolate the exact point on that segment
4. Create two new lines:
   - Line 1: from start to snapped point
   - Line 2: from snapped point to end
5. Preserve original attributes in both resulting lines
**Validation**: Returns list of 2 line segments with original attributes

#### Step 1B: `split_line_at_two_points(line, pt1_coords, pt2_coords)` - Lines 260-347
**Purpose**: Split a single LINESTRING at two point locations (when both points snap to same line)
**Key operations**:
1. Extract coordinates and find positions of both points on the line
2. Ensure pt1 comes before pt2 along the line direction
3. Create three segments:
   - Segment 1: from start to pt1
   - Segment 2: from pt1 to pt2 (the middle segment)
   - Segment 3: from pt2 to end
4. Preserve original attributes in all three segments
**Special handling**: Handle edge cases where points are on same segment or at endpoints
**Validation**: Returns list of 3 line segments

#### Step 1C: `split_streamlines_at_points(streamlines, snapped_pt1, snapped_pt2)` - Lines 149-206
**Purpose**: Apply point splitting to the entire streamlines layer at snapped locations
**Key operations**:
1. Identify which rows need splitting based on snapped point line indices
2. Handle two cases:
   - **Different lines**: Split line 1 at pt1, split line 2 at pt2
   - **Same line**: Split that line at both pt1 and pt2 (using `split_line_at_two_points()`)
3. Carefully manage row indices when adding new rows (update idx2 if needed)
4. Reassemble the streamlines dataframe with split segments
5. Preserve all original attributes
**Output**: streamlines dataframe with additional rows for split segments
**Validation**: Correct number of rows, all attributes preserved

---

### **Phase 2: Update Main Workflow in `extract_stream_path()`** (Lines 48-125)

Reorganize the function to follow the correct workflow:

#### Step 2A: Preserve Input Validation (Lines 50-62)
- No changes needed - checks are correct

#### Step 2B: Update CRS Handling (Lines 64-70)
- No changes needed - logic is correct

#### Step 2C: Cast to LINESTRING (Lines 72-73)
- No changes needed - handles MULTILINESTRING correctly

#### Step 2D: Snap Points to Streamlines (Lines 75-77)
**Current status**: Working correctly
**Key detail**: `snap_point_to_streamline()` must return snapped coordinates

#### Step 2E: **ADD** Special Case for Same Line (Lines 79-97)
**Current Issue**: This case exists but doesn't work properly
**Fix approach**:
- Keep the logic but improve it to return the proper clipped segment
- When both points snap to same line (idx1 == idx2), don't return a direct line
- Instead, fall through to the normal workflow which will now handle it correctly via splitting

#### Step 2F: **ADD NEW STEP** - Split Streamlines (New Lines 99-104)
**Purpose**: This is the critical missing step
```
streamlines_split <- split_streamlines_at_points(
  streamlines_cast,
  snapped_pt1,
  snapped_pt2
)
```
**Impact**: Creates network-ready streamlines with split segments positioned at snap points

#### Step 2G: Add Metadata (Lines 105-108)
**Purpose**: Prepare data for network shortest path algorithm
```
streamlines_split$length_m <- as.numeric(st_length(streamlines_split))
streamlines_split$edge_id <- seq_len(nrow(streamlines_split))
```
**Key detail**: `edge_id` enables tracking which network edge corresponds to which row

#### Step 2H: Create Network (Lines 110-111)
**Status**: Mostly correct, but now uses split streamlines instead of original

#### Step 2I: Find Shortest Path (Lines 113-114)
**Status**: Function works, but needs to be updated to return row indices

#### Step 2J: Extract Path Segments (Lines 117-122)
**Status**: Needs complete rewrite (see Phase 3)

---

### **Phase 3: Add Segment Clipping Functions** (Lines 476-632)

These functions handle the precise clipping of start/end segments:

#### Step 3A: `clip_line_from_start(line, point_coords)` - Lines 476-518
**Purpose**: Remove the start of a line up to a point, keep point onwards
**Visual**:
```
Original: ●─────●─────●─────●
Point:         ✕
Result:        ✓─────●─────●
```
**Key operations**:
1. Find which segment contains the snapped point
2. Create new coordinates from that segment onwards to end
3. Insert the snapped point as first coordinate
4. Preserve attributes
**Usage**: Clipping first segment in a multi-segment path

#### Step 3B: `clip_line_to_end(line, point_coords)` - Lines 521-563
**Purpose**: Remove the end of a line after a point, keep start to point
**Visual**:
```
Original: ●─────●─────●─────●
Point:              ✕
Result:    ●─────●─────✓
```
**Key operations**:
1. Find which segment contains the snapped point
2. Create new coordinates from start to that segment
3. Append the snapped point as last coordinate
4. Preserve attributes
**Usage**: Clipping last segment in a multi-segment path

#### Step 3C: `clip_line_between_points(line, pt1_coords, pt2_coords)` - Lines 566-632
**Purpose**: Extract only the portion between two snapped points
**Visual**:
```
Original: ●─────●─────●─────●─────●
Points:        ✕          ✕
Result:        ✓──────────✓
```
**Key operations**:
1. Find positions of both points on the line
2. Ensure pt1 comes before pt2
3. Create coordinates from pt1 to pt2 (including intermediate vertices)
4. Preserve attributes
**Usage**: When both points snap to the same segment (single-segment path)

---

### **Phase 4: Rewrite `find_shortest_path_network()` Function** (Lines 361-404)

**Current Issue**: Returns node indices instead of edge/row indices
**Fix approach**:

#### Step 4A: Keep Network and Node Processing (Lines 367-376)
- Extract nodes from network
- Find nearest nodes to snapped points
- No changes needed

#### Step 4B: **CRITICAL FIX** - Return Edge Indices Not Node Indices (Lines 378-403)
**Current code**:
```r
edge_indices <- paths$edge_paths[[1]]
row_indices <- edge_indices  # ← This is wrong! Edge indices ≠ streamline row indices
```

**Fixed code**:
```r
edge_indices <- paths$edge_paths[[1]]
# Since as_sfnetwork() preserves edge order:
# edge_index i refers to row i in the input streamlines dataframe
row_indices <- edge_indices  # Now this is correct because edges map 1:1 to rows
```

**Key insight**: `sfnetwork::as_sfnetwork()` maintains the original row order as edge order, so direct mapping works

**Return value**: Correct `edge_path` values that correspond to row numbers in `streamlines_split`

---

### **Phase 5: Rewrite `extract_path_segments()` Function** (Lines 407-473)

**Current Issue**: Tries to use node indices as streamline row indices
**Complete rewrite**:

#### Step 5A: Get Edge Indices (Lines 411-420)
```r
edge_indices <- path_result$edge_path
# These are now row indices in streamlines_split
path_segments <- streamlines[edge_indices, , drop = FALSE]
```

#### Step 5B: Loop Through Each Edge in Path (Lines 437-462)
For each segment in the path:
1. Determine position: first, middle, or last
2. Apply appropriate clipping:
   - **First segment ONLY**: `clip_line_from_start(segment, pt1_coords)`
   - **Last segment ONLY**: `clip_line_to_end(segment, pt2_coords)`
   - **First AND Last** (single segment): `clip_line_between_points(segment, pt1_coords, pt2_coords)`
   - **Middle segments**: Keep entire geometry
3. Append clipped segment to list

#### Step 5C: Combine Results (Lines 469-472)
```r
result <- do.call(rbind, all_segments)
```
This preserves all attributes (especially `rid`) from individual segments

---

### **Phase 6: Update Helper Function `snap_point_to_streamline()`** (Lines 128-146)

**Current status**: Mostly correct
**Needed addition**: Return coordinates alongside snapped point
```r
list(
  point = snapped,
  line_idx = nearest_idx,
  original_line = nearest_line,
  coords = sf::st_coordinates(snapped)[1, 1:2]  # ← Add this
)
```

---

### **Phase 7: Simplify `create_stream_network()`** (Lines 350-358)

**Current status**: Works fine
**No major changes needed**

---

## Test Validation Criteria

The tests in `test-extract-stream-path.R` verify:

1. **Path Length Accuracy**:
   - `abs(expected_length - result_length) < 2` meters
   - Tests with 5 different scenarios: PEP001, PEP17, PEP20, PEP99, PEP100

2. **Segment Preservation**:
   - `all(expected_rids %in% result_rids)` - All expected stream segments present
   - `all(result_rids %in% expected_rids)` - No extra unwanted segments

---

## Implementation Order (Sequential)

1. **Phase 1**: Implement splitting functions (prerequisite for everything else)
2. **Phase 6**: Update `snap_point_to_streamline()` to return coordinates
3. **Phase 2**: Update main `extract_stream_path()` workflow (add split step, remove bad same-line case)
4. **Phase 4**: Fix `find_shortest_path_network()` to return row indices
5. **Phase 3**: Implement clipping functions (need splitting to work first)
6. **Phase 5**: Rewrite `extract_path_segments()` to use new clipping functions
7. **Phase 7**: Verify `create_stream_network()` works as-is

---

## Key Technical Insights

### Why Splitting is Critical
- Network nodes must align with snapped point locations
- Without splitting, snap points may fall in the middle of edges, causing incorrect node positioning
- Network algorithm then finds paths using wrong node connections

### Why Row Index Mapping Works
- `sfnetwork::as_sfnetwork(streamlines, directed = FALSE)` preserves row order
- Edge #i in the network corresponds exactly to row i in the input sf dataframe
- This eliminates the need for complex edge-to-feature lookup tables

### Attribute Preservation Strategy
- Each split/clip operation uses `sf::st_drop_geometry(original_line)` to copy attributes
- This ensures `rid` and other columns follow through all transformations
- Final result has all segments with their original attributes intact

---

## Common Pitfalls to Avoid

1. **Off-by-one errors in coordinate slicing**: Use explicit parentheses `(idx + 1):n` not `idx + 1:n`
2. **Losing attributes**: Always use `sf::st_drop_geometry()` when creating new geometries
3. **Confusing node vs. edge indices**: Remember sfnetwork edge i = streamline row i
4. **Geometry gaps**: Ensure clipping preserves connectivity at snapped points
5. **Not handling edge cases**: Points on segment endpoints, same-line paths, etc.

---

## Debugging Strategy if Tests Still Fail

1. Use test debug scripts already in repo (test_debug_pep17.R, etc.)
2. Check snapped coordinates match expected snap locations
3. Verify split streamlines have correct number of rows
4. Confirm network has correct nodes at split locations
5. Print path edges to verify correct segments are selected
6. Check clipped geometries don't have gaps at snap points
7. Validate final result rids match expected values

