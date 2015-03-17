-record(capture, {tree, time}).
-record(node, {type, name, proc_info, children = []}).
-record(env, {mode, time, node_stats, header, body, footer, x, y, x_max, y_max, cursor_y, shift_y, marked_node, toggle_open, current_sup_pid, open_pids, body_height}).
-record(node_stats, {reductions, memory, message_queue_len}).

-define(BLACK, 0).
-define(WHITE, 1).
-define(RED, 2).
-define(GREEN, 3).
-define(BLUE, 4).
-define(YELLOW, 5).
-define(RED_PALE, 6).
-define(YELLOW_PALE, 7).
-define(BLUE_PALE, 8).
-define(DARKGRAY, 9).

-define(WHITE_TYPE, 101).
-define(GREEN_TYPE, 102).
-define(YELLOW_TYPE, 103).
-define(RED_TYPE, 104).
-define(WHITE_HL_TYPE, 105).
-define(GREEN_HL_TYPE, 106).
-define(YELLOW_HL_TYPE, 107).
-define(RED_HL_TYPE, 108).
-define(BLUE_TYPE, 109).
-define(BLUE_HL_TYPE, 110).

-define(RED_PALE_TYPE, 111).
-define(YELLOW_PALE_TYPE, 112).
-define(BLUE_PALE_TYPE, 113).


-define(CURSORCOLOR, 114).
-define(CURSOR_HL, 115).

-define(POOL_TYPE, 116).

