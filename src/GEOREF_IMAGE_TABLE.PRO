;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Get_Label_Dims @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION Get_Label_Dims, LABEL_ID = label_id
   in_label = 1
   IF NOT KEYWORD_SET(label_id) THEN BEGIN
      in_label = 0
      b        = WIDGET_BASE(/COLUMN)
      label_id = WIDGET_LABEL(b, VALUE = 'test label')
   ENDIF

   geom = WIDGET_INFO(label_id, /GEOMETRY)

   IF NOT in_label THEN RETURN, geom.scr_ysize+(2*geom.margin)

   RETURN, [ geom.scr_xsize+(2*geom.margin), geom.scr_ysize+(2*geom.margin) ]

END
; Get_Label_Dims

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ Get_Table_Cell_XY_Dims @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION Get_Table_Cell_XY_Dims
   b    = WIDGET_BASE(/COLUMN)
   t1   = WIDGET_TABLE( b,                       $
                        VALUE = BINDGEN(10,10),  $
                        X_SCROLL_SIZE = 2,       $
                        Y_SCROLL_SIZE = 2,       $
                        XSIZE = 10,              $
                        YSIZE = 10 )

   g1   = WIDGET_INFO( t1, /GEOMETRY )
   t1_x = g1.scr_xsize+(2*g1.margin)
   t1_y = g1.scr_ysize+(2*g1.margin)

   t2   = WIDGET_TABLE( b,                       $
                        VALUE = BINDGEN(10,10),  $
                        X_SCROLL_SIZE = 3,       $
                        Y_SCROLL_SIZE = 3,       $
                        XSIZE = 10,              $
                        YSIZE = 10 )
   g2   = WIDGET_INFO( t2, /GEOMETRY )
   t2_x = g2.scr_xsize+(2*g2.margin)
   t2_y = g2.scr_ysize+(2*g2.margin)

   ;
   ; cell width, cell height, non-cell x, non-cell y
   ;
   cell_xy = LONG([ t2_x - t1_x, t2_y - t1_y, t2_x - 3*(t2_x - t1_x), t2_y - 3*(t2_y - t1_y) ])

   WIDGET_CONTROL, t1, /DESTROY
   WIDGET_CONTROL, t2, /DESTROY
   WIDGET_CONTROL, b, /DESTROY

   RETURN, cell_xy
END
; Get_Table_Cell_XY_Dims

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE_TABLE::GetTableTitles @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE_TABLE::GetTableTitles
   RETURN, *(SELF.table_titles_ptr)
END
; GEOREF_IMAGE_TABLE::GetTableTitles

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE_TABLE::GetTableOnOffState @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE_TABLE::GetTableOnOffState
   RETURN, *(SELF.table_onoff_ptr)
END
; GEOREF_IMAGE_TABLE::GetTableOnOffState

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@ get_active_table_indices_ev @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO get_active_table_indices_ev, event
   WIDGET_CONTROL, event.id, GET_UVALUE = button_name

   CASE button_name OF
      'set': BEGIN
         WIDGET_CONTROL, event.top, /DESTROY
         END
      ELSE: BEGIN
         button_base = WIDGET_INFO(event.id, /PARENT)
         WIDGET_CONTROL, button_base, GET_UVALUE = max_tables
         WIDGET_CONTROL, event.id, GET_UVALUE = str_button_idx
         WIDGET_CONTROL, event.top, GET_UVALUE = out_ptr

         IF event.select AND LONG(TOTAL((*(out_ptr)))) EQ max_tables THEN BEGIN
            WIDGET_CONTROL, event.id, SET_BUTTON = 0
            msg = [ 'Cannot have more than ' + STRTRIM(STRING(FIX(max_tables)),2), $
                    'tables active at one time.' ]
            ret = DIALOG_MESSAGE(msg, /INFORMATION)
            RETURN
         ENDIF

         IF NOT event.select AND LONG(TOTAL((*(out_ptr)))) EQ 1 THEN BEGIN
            WIDGET_CONTROL, event.id, /SET_BUTTON
            msg = [ 'Must have at least 1 table active at all times.' ]
            ret = DIALOG_MESSAGE(msg, /INFORMATION)
            RETURN
         ENDIF

         button_idx = FIX(str_button_idx)
         (*(out_ptr))[button_idx] = event.select
         END
   ENDCASE
END
; get_active_table_indices_ev

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ Get_Active_Table_Indices @@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION Get_Active_Table_Indices, table_names, table_onoff_toggle, group_leader, max_tables
   tlb  = WIDGET_BASE( GROUP_LEADER = group_leader, /MODAL, /COLUMN )
   lab1 = WIDGET_LABEL( tlb, VALUE = 'Table Display Controls', FRAME = 3 )
   sub1 = WIDGET_BASE( tlb, /NONEXCLUSIVE, /COLUMN )

   b = LONARR(N_ELEMENTS(table_names))

   FOR i = 0, N_ELEMENTS(table_names) - 1 DO $
      b[i] = WIDGET_BUTTON( sub1, VALUE = table_names[i], UVALUE = STRTRIM(STRING(i),2) )

   FOR i = 0, N_ELEMENTS(table_names) - 1 DO $
      IF table_onoff_toggle[i] THEN WIDGET_CONTROL, b[i], /SET_BUTTON

   set_button = WIDGET_BUTTON( tlb, VALUE = 'Set', UVALUE = 'set' )

   WIDGET_CONTROL, tlb, /REALIZE

   WIDGET_CONTROL, sub1, SET_UVALUE = max_tables

   out_ptr = PTR_NEW( table_onoff_toggle, /NO_COPY )

   WIDGET_CONTROL, tlb, SET_UVALUE = out_ptr

   XMANAGER, 'Get_Active_Table_Indices', tlb, EVENT_HANDLER = 'get_active_table_indices_ev'

   ret = *out_ptr
   PTR_FREE, out_ptr
   RETURN, ret
END
; Get_Active_Table_Indices

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE_TABLE::GetMaxTables @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE_TABLE::GetMaxTables
   RETURN, SELF.max_tables
END
; GEOREF_IMAGE_TABLE::GetMaxTables

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE_TABLE::GetDims @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE_TABLE::GetDims
   RETURN, [SELF.xsize, SELF.ysize]
END
; GEOREF_IMAGE_TABLE::GetDims

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE_TABLE::SetTableOnOffState @@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE_TABLE::SetTableOnOffState, toggle_list
   PTR_FREE, SELF.table_onoff_ptr
   SELF.table_onoff_ptr = PTR_NEW(toggle_list, /NO_COPY)
END
; GEOREF_IMAGE_TABLE::SetTableOnOffState

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@ georef_image_table_ev @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION georef_image_table_ev, event
;print,tag_names(event)
   what_widget = STRUPCASE( TAG_NAMES( event, /STRUCTURE_NAME ) )
   type       = (-1)
   sel_left   = (-1)
   sel_top    = (-1)
   sel_right  = (-1)
   sel_bottom = (-1)
   table_idx  = (-1)

   CASE what_widget OF
      'WIDGET_TABLE_CELL_SEL': BEGIN
;   print,'widget_table event'
         type       = event.type
         sel_left   = event.sel_left
         sel_top    = event.sel_top
         sel_right  = event.sel_right
         sel_bottom = event.sel_bottom
         WIDGET_CONTROL, event.id, GET_UVALUE = table_idx
         END
      'WIDGET_BUTTON': BEGIN
         WIDGET_CONTROL, event.id, GET_UVALUE = menu_item_name
         menu_id = WIDGET_INFO(event.id, /PARENT)
         WIDGET_CONTROL, menu_id, GET_UVALUE = obj
         CASE menu_item_name OF
            'tabledisplayoptions': BEGIN
               max_tables = obj->GetMaxTables()
               orig_onoff_state = obj->GetTableOnOffState()
               new_onoff_idx    = Get_Active_Table_Indices(obj->GetTableTitles(), $
                  obj->GetTableOnOffState(), event.top, max_tables)
               change_idx = WHERE(orig_onoff_state NE new_onoff_idx, cnt)
               IF cnt GT 0 THEN BEGIN
                  obj_xysize = obj->GetDims()
                  obj->SetTableOnOffState, new_onoff_idx
                  obj->Resize, obj_xysize[0], obj_xysize[1]
               ENDIF
               END
            ELSE:
         ENDCASE
         END
      ELSE:
   ENDCASE

   RETURN, { GEOREFIMAGETABLE,               $
             id          :event.id,          $
             top         :event.top,         $
             handler     :event.handler,     $
             what_widget :what_widget,       $
             type        :type,              $
             sel_left    :sel_left,          $
             sel_top     :sel_top,           $
             sel_right   :sel_right,         $
             sel_bottom  :sel_bottom,        $
             table_idx   :table_idx }
END
; georef_image_table_ev


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Build_Table_Object @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION Build_Table_Object, parent_base, data_ptr_arr,                                 $
                             TABLE_TITLES = table_titles, COLUMN = column, ROW = row,   $
                             XSIZE = xsize, YSIZE = ysize, GROUP_LEADER = group_leader, $
                             FRAME = frame, ADDED_MENU_ITEMS = added_menu_items
;help,added_menu_items
;print,'xsize,ysize = ',xsize,ysize
   IF NOT KEYWORD_SET(row) THEN                                           $
      obj_base = WIDGET_BASE( parent_base,                                $
                                   /COLUMN,                               $
                                   XSIZE = xsize,                         $
                                   YSIZE = ysize,                         $
                                   /BASE_ALIGN_CENTER,                    $
                                   EVENT_FUNC = 'georef_image_table_ev' ) $
   ELSE                                                                   $
      obj_base = WIDGET_BASE( parent_base,                                $
                                   /ROW,                                  $
                                   XSIZE = xsize,                         $
                                   YSIZE = ysize,                         $
                                   /BASE_ALIGN_CENTER,                    $
                                   EVENT_FUNC = 'georef_image_table_ev' )

   control_button = WIDGET_BUTTON( obj_base, /MENU, VALUE = 'Controls' )

   geom           = WIDGET_INFO( control_button, /GEOMETRY )
   control_button_xsize = geom.scr_xsize+(2*geom.margin)
   control_button_ysize = geom.scr_ysize+(2*geom.margin)

   item1 = WIDGET_BUTTON( control_button,                     $
                          VALUE = 'Table Display Options...', $
                          UVALUE = 'tabledisplayoptions' )

   IF KEYWORD_SET(added_menu_items) THEN BEGIN
; print,'processing added items'
      FOR j = 0, N_ELEMENTS(added_menu_items[0,*]) - 1 DO BEGIN
         added_item = WIDGET_BUTTON( control_button,                     $
                                VALUE = added_menu_items[0,j], $
                                UVALUE = added_menu_items[1,j] )
      ENDFOR
   ENDIF

   row_flag = 0

   IF KEYWORD_SET(row) THEN row_flag = 1

   n_datasets              = N_ELEMENTS( data_ptr_arr )

   ;---------------------------------------------------------------
   ; Set up initial values of table_base_xsize and table_base_ysize to
   ; be slightly smaller than the input base xsize and ysize values
   ;---------------------------------------------------------------
   table_base_xsize        = xsize
   table_base_ysize        = ysize

   trim_extra              = 5 * frame

   IF KEYWORD_SET(row) THEN BEGIN
      table_base_xsize     =                                                    $
         ( table_base_xsize - control_button_xsize ) / n_datasets - trim_extra
       table_base_ysize    =  table_base_ysize- trim_extra
  ENDIF ELSE BEGIN
      table_base_xsize     = table_base_xsize - trim_extra - 15
      table_base_ysize     =                                                    $
         ( table_base_ysize - control_button_ysize ) / n_datasets - trim_extra - 5
   ENDELSE

   b                       = LONARR( n_datasets )
   t                       = LONARR( n_datasets )

   cell_xy_dims            = Get_Table_Cell_XY_Dims()
   label_y_dims            = Get_Label_Dims()

   cell_x_size             = cell_xy_dims[0]
   cell_y_size             = cell_xy_dims[1]

   table_x_noncell         = cell_xy_dims[2]
   table_y_noncell         = cell_xy_dims[3]

   INITIAL_SCROLL_X = LONG( ((table_base_xsize - table_x_noncell) / cell_x_size) )

   INITIAL_SCROLL_Y = LONG( ((table_base_ysize - table_y_noncell - label_y_dims) / cell_y_size) - 1 )

   FOR i = 0, n_datasets - 1 DO BEGIN

      b[i]          = WIDGET_BASE( obj_base,                               $
                                   XSIZE = table_base_xsize,               $
                                   YSIZE = table_base_ysize,               $
                                   /COLUMN,                                $
                                   FRAME = frame,                          $
                                   /BASE_ALIGN_CENTER )

      title         = WIDGET_LABEL( b[i],                                  $
                                    VALUE = table_titles[i] )

      t[i]          = WIDGET_TABLE( b[i],                                  $
                                    VALUE = *(data_ptr_arr[i]),            $
                                    X_SCROLL_SIZE = INITIAL_SCROLL_X,      $
                                    Y_SCROLL_SIZE = INITIAL_SCROLL_Y,      $
                                    XSIZE = (SIZE(*(data_ptr_arr[i])))[1], $
                                    YSIZE = (SIZE(*(data_ptr_arr[i])))[2], $
                                    /RESIZEABLE_COLUMNS,                   $
                                    UVALUE = i,                            $
                                    /ALL_EVENTS )
   ENDFOR

   table_id_ptr    = PTR_NEW(t, /NO_COPY)
   table_base_ptr  = PTR_NEW(b, /NO_COPY)

   RETURN, { obj_base          :obj_base,       $
             row_flag          :row_flag,       $
             table_id_ptr      :table_id_ptr,   $
             control_button    :control_button, $
             table_base_ptr    :table_base_ptr }
END
; Build_Table_Object

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE_TABLE::GetMinBaseDims @@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE_TABLE::GetMinBaseDims
   DEVICE, GET_SCREEN_SIZE = screen_xy_dims

   screen_x_size           = screen_xy_dims[0]
   screen_y_size           = screen_xy_dims[1]

   cell_xy_dims            = Get_Table_Cell_XY_Dims()
   label_y_dims            = Get_Label_Dims()

   cell_x_size             = cell_xy_dims[0]
   cell_y_size             = cell_xy_dims[1]
   table_x_noncell         = cell_xy_dims[2]
   table_y_noncell         = cell_xy_dims[3]
   n_datasets              = N_ELEMENTS( *(SELF.data_ptr) )

   control_button_id       = WIDGET_INFO( SELF.obj_base, /CHILD )
   control_button_xy_dims  = Get_Label_Dims(LABEL_ID = control_button_id)

   trim_extra = 5 * SELF.frame

   ;
   ; 15 and 5 are added to facilitate the appearance of the tables on Windows platforms
   ; If not added, the frames will ppear to be truncated.
   ;
   min_x      = cell_x_size + table_x_noncell + trim_extra + 15
   min_y      = (2L*cell_y_size) + label_y_dims + table_y_noncell + trim_extra + 15
   done       = 0

   max_tables = n_datasets

   WHILE NOT done DO BEGIN
     IF SELF.row_flag THEN BEGIN
         new_min_x = (min_x * max_tables) + control_button_xy_dims[0]
         new_min_y = min_y
      ENDIF ELSE BEGIN
         new_min_x = min_x
         new_min_y = (min_y * max_tables) + control_button_xy_dims[1]
      ENDELSE
      IF new_min_x GT screen_x_size OR new_min_y GT screen_y_size THEN $
         max_tables = max_tables - 1                           $
      ELSE done = 1
   ENDWHILE

   RETURN, [ new_min_x, new_min_y, max_tables ]
END
; GEOREF_IMAGE_TABLE::GetMinBaseDims

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE_TABLE::Resize @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE_TABLE::Resize, new_scr_xsize, new_scr_ysize

   n_active_datasets_idx = WHERE( *(SELF.table_onoff_ptr) GT 0, cnt )

   CASE 1 OF
      SELF.row_flag AND SELF.group_leader GT 0:     e = { ROW:1, GROUP_LEADER:SELF.group_leader }
      SELF.row_flag AND SELF.group_leader LE 0:     e = { ROW:1 }
      NOT SELF.row_flag AND SELF.group_leader GT 0: e = { COLUMN:1, GROUP_LEADER:SELF.group_leader }
      NOT SELF.row_flag AND SELF.group_leader LE 0: e = { COLUMN:1 }
      ELSE:
   ENDCASE

   WIDGET_CONTROL, SELF.obj_base, /DESTROY

   IF PTR_VALID(SELF.added_menu_items_ptr) THEN e = {FRAME:SELF.frame, ADDED_MENU_ITEMS:*(SELF.added_menu_items_ptr)} $
   ELSE e = {FRAME:SELF.frame}

   ret_struct = Build_Table_Object( SELF.parent_base, (*(SELF.data_ptr))[n_active_datasets_idx], $
                                    TABLE_TITLES = (*(SELF.table_titles_ptr))[n_active_datasets_idx],                 $
                                    XSIZE = new_scr_xsize, YSIZE = new_scr_ysize, $
                                    _EXTRA = e )

   SELF.xsize            = new_scr_xsize
   SELF.ysize            = new_scr_ysize
   SELF.obj_base         = ret_struct.obj_base
   PTR_FREE, SELF.table_id_ptr
   SELF.table_id_ptr     = ret_struct.table_id_ptr
   PTR_FREE, SELF.table_base_ptr
   SELF.table_base_ptr   = ret_struct.table_base_ptr

   WIDGET_CONTROL, ret_struct.control_button, SET_UVALUE = SELF
   WIDGET_CONTROL, ret_struct.obj_base, SET_UVALUE = SELF

END
;GEOREF_IMAGE_TABLE::Resize

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE_TABLE::INIT @@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
FUNCTION GEOREF_IMAGE_TABLE::INIT, parent_base,                 $
                                   data_ptr_arr,                $
                                   TABLE_TITLES = table_titles, $
                                   COLUMN = column,             $
                                   ROW = row,                   $
                                   XSIZE = xsize,               $
                                   YSIZE = ysize,               $
                                   GROUP_LEADER = group_leader, $
                                   FRAME = frame,               $
                                   MAX_DISPLAYED_TABLES = max_tables, $
                                   ADDED_MENU_ITEMS = added_menu_items

   IF N_PARAMS() LT 2 THEN RETURN, 0

   IF NOT KEYWORD_SET(frame) THEN SELF.frame = 3 ELSE SELF.frame = frame

   n_datasets = N_ELEMENTS( data_ptr_arr )

   IF NOT KEYWORD_SET(table_titles) THEN table_titles = STRARR(n_datasets) + $
                                                        'Table #' +          $
                                                        STRTRIM(STRING(INDGEN(n_datasets)+1),2)

   IF N_ELEMENTS(table_titles) LT n_datasets THEN BEGIN
      old_table_titles = table_titles
      table_titles     = STRARR(n_datasets)
      table_titles[0:N_ELEMENTS(old_table_titles)-1] = old_table_titles
      table_titles[N_ELEMENTS(old_table_titles):n_datasets-1] = $
         STRARR(n_datasets-N_ELEMENTS(old_table_titles)) + 'Table #' + $
         STRTRIM(STRING(INDGEN(n_datasets-N_ELEMENTS(old_table_titles))+N_ELEMENTS(old_table_titles)+1),2)
   ENDIF

   SELF.data_ptr         = PTR_NEW(data_ptr_arr)
   SELF.table_titles_ptr = PTR_NEW(table_titles)

   in_column = 0
   in_row    = 0
   IF NOT KEYWORD_SET(row) THEN in_column = 1 ELSE in_row = 1

   SELF.row_flag = in_row

   SELF.obj_base = WIDGET_BASE( COLUMN = in_column, ROW = in_row )
   control_button = WIDGET_BUTTON(SELF.obj_base, VALUE = 'Controls' )

   min_xy = SELF->GetMinBaseDims()

   WIDGET_CONTROL, SELF.obj_base, /DESTROY

   SELF.table_onoff_ptr = PTR_NEW( INTARR(n_datasets) + 1, /NO_COPY )

   IF NOT KEYWORD_SET(max_tables) THEN max_tables = min_xy[2]

   IF max_tables GT min_xy[2] THEN max_tables = min_xy[2]

   IF min_xy[2] LT n_datasets THEN BEGIN
      msg = ['Not all tables for all datasets can be displayed simultaneously.',          $
             'Only the tables for the first ' + STRTRIM(STRING(FIX(max_tables)),2) + $
             ' datasets will be displayed.' ]
      ret = DIALOG_MESSAGE( msg, /INFORMATION )
      (*(SELF.table_onoff_ptr))[max_tables:n_datasets-1] = 0
   ENDIF

   SELF.max_tables = max_tables

   IF NOT KEYWORD_SET(xsize) THEN xsize = min_xy[0]
   IF NOT KEYWORD_SET(ysize) THEN ysize = min_xy[1]

;   IF xsize LT min_xy[0] THEN xsize = min_xy[0]
;   IF ysize LT min_xy[1] THEN ysize = min_xy[1]

   IF KEYWORD_SET(group_leader) THEN SELF.group_leader = group_leader ELSE SELF.group_leader = (-1L)

   n_active_datasets_idx = WHERE( *(SELF.table_onoff_ptr) GT 0, cnt )

   IF KEYWORD_SET(added_menu_items) THEN BEGIN
      e = { FRAME:SELF.frame, ADDED_MENU_ITEMS:added_menu_items}
      SELF.added_menu_items_ptr = PTR_NEW(added_menu_items,/NO_COPY)
   ENDIF ELSE BEGIN
      e = { FRAME:SELF.frame}
   ENDELSE

;   print,tag_names(e)

   ret_struct = Build_Table_Object( parent_base, (*(SELF.data_ptr))[n_active_datasets_idx],    $
                                    TABLE_TITLES = table_titles[n_active_datasets_idx],        $
                                    COLUMN = column, ROW = row,                                $
                                    XSIZE = xsize, YSIZE = ysize, GROUP_LEADER = group_leader, $
                                    _EXTRA = e )
   SELF.xsize            = xsize
   SELF.ysize            = ysize
   SELF.data_ptr         = PTR_NEW(data_ptr_arr)
   SELF.table_titles_ptr = PTR_NEW(table_titles,/NO_COPY)
   SELF.parent_base      = parent_base
   SELF.obj_base         = ret_struct.obj_base
   SELF.row_flag         = ret_struct.row_flag
   SELF.table_id_ptr     = ret_struct.table_id_ptr
   SELF.table_base_ptr   = ret_struct.table_base_ptr

   WIDGET_CONTROL, ret_struct.control_button, SET_UVALUE = SELF
   WIDGET_CONTROL, ret_struct.obj_base, SET_UVALUE = SELF

   RETURN, 1
END
; GEOREF_IMAGE_TABLE::INIT

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE_TABLE::CLEANUP @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE_TABLE::CLEANUP
   PTR_FREE, SELF.table_id_ptr
   PTR_FREE, SELF.table_titles_ptr
   PTR_FREE, SELF.data_ptr
   PTR_FREE, SELF.table_base_ptr
   PTR_FREE, SELF.table_onoff_ptr
   PTR_FREE, SELF.added_menu_items_ptr
END
; GEOREF_IMAGE_TABLE::CLEANUP

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@ GEOREF_IMAGE_TABLE__DEFINE @@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
PRO GEOREF_IMAGE_TABLE__DEFINE
   GEOREF_IMAGE_TABLE = { GEOREF_IMAGE_TABLE,               $
                          obj_base        :0L,              $
                          parent_base     :0L,              $
                          group_leader    :0L,              $
                          row_flag        :0,               $
                          xsize           :0L,              $
                          ysize           :0L,              $
                          frame           :0,               $
                          max_tables      :0,               $
                          data_ptr        :PTR_NEW(),       $
                          table_titles_ptr:PTR_NEW(),       $
                          table_id_ptr    :PTR_NEW(),       $
                          table_base_ptr  :PTR_NEW(),       $
                          table_onoff_ptr :PTR_NEW(),       $
                          added_menu_items_ptr :PTR_NEW()   $
                        }
END
; GEOREF_IMAGE_TABLE__DEFINE
