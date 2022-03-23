CLASS zcl_ca_bw_query_bex DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF cs_suffix_value_field,
                 currency TYPE string VALUE '_CURRENCY',
                 unit     TYPE string VALUE '_UNIT',
               END OF cs_suffix_value_field.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_infoprovider | <p class="shorttext synchronized">Infoprovider</p>
    "! @parameter iv_query | <p class="shorttext synchronized">Query</p>
    METHODS constructor
      IMPORTING iv_infoprovider TYPE rsinfoprov
                iv_query        TYPE rszcompid.

    "! <p class="shorttext synchronized">Guarda los parámetros</p>
    "! Los parámetros se alimen de la siguiente manera:
    "! Si tenemos un variable de tipo rango la variable se rellena así:
    "! name = 'VAR_NAME_x' value = |nombre variable|
    "! name = 'VAR_SIGN_x' value = 'I'
    "! name = 'VAR_OPERATOR_x' value = 'EQ'
    "! name = 'VAR_VALUE_LOW_EXT_x' value = |valor| )
    "! Donde "x" es el contador donde se agrupan las variables. La primera que pongamos será 1, la
    "! siguiente 2, y así.
    "! Si tenemos un variable que no es rango pero permite multiples valores o solo permite
    "! uno valor se rellena así:
    "! name = 'VAR_NAME_x' value = |nombre variable|
    "! name = 'VAR_VALUE_LOW_EXT_x' value = |valor| )
    "! Si la variable permite  multiple valores podremos usar el mismo nombre de variables pero incremento
    "! el numero de variables.
    "! Un ejemplo de llamada sería así:
    "! ( name = 'VAR_NAME_1' value = 'ZXXX_VAR_REPCOMP_MULTI_MAND' )
    "! ( name = 'VAR_VALUE_EXT_1' value = 'XX45' )
    "! ( name = 'VAR_NAME_2' value = 'ZXXX_VAR_PERIOD_RANG' )
    "! ( name = 'VAR_SIGN_2' value = 'I' )
    "! ( name = 'VAR_OPERATOR_2' value = 'EQ' )
    "! ( name = 'VAR_VALUE_LOW_EXT_2' value = '1' )
    "! ( name = 'VAR_NAME_3' value = 'ZXXX_VAR_YEAR_MAND' )
    "! ( name = 'VAR_VALUE_EXT_3' value = '2021' )
    "! @parameter it_parameters | <p class="shorttext synchronized">Valores de las variables</p>
    METHODS set_parameters
      IMPORTING it_parameters TYPE rrxw3tquery.

    "! <p class="shorttext synchronized">Ejecutar query</p>
    "! @parameter it_parameters | <p class="shorttext synchronized">Valores de las variables</p>
    "! @parameter eo_values | <p class="shorttext synchronized">Valores</p>
    "! @parameter et_fieldcat | <p class="shorttext synchronized">Catalogo de campos</p>
    "! @parameter es_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS execute
      IMPORTING it_parameters TYPE rrxw3tquery OPTIONAL
      EXPORTING eo_values     TYPE REF TO data
                et_fieldcat   TYPE lvc_t_fcat
                es_return     TYPE bapiret2.
  PROTECTED SECTION.

    DATA mt_cell_data  TYPE rrws_t_cell.
    DATA mt_axis_info  TYPE rrws_thx_axis_info.
    DATA mt_axis_data  TYPE rrws_thx_axis_data.
    DATA mt_parameters TYPE rrxw3tquery.
    DATA mv_infoprovider  TYPE rsinfoprov.
    DATA mv_query TYPE rszcompid.
    DATA mt_itab_comp TYPE cl_abap_structdescr=>component_table.
    DATA mt_fieldcat TYPE lvc_t_fcat.
    DATA mo_struct_type TYPE REF TO cl_abap_structdescr.
    DATA mo_struct_data TYPE REF TO data.
    DATA mo_itab_type  TYPE REF TO cl_abap_tabledescr.
    DATA mo_itab_data  TYPE REF TO data.

    "! <p class="shorttext synchronized">Obtener valores de la query</p>
    "! @parameter es_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS get_query_data
      EXPORTING es_return TYPE bapiret2.
    "! <p class="shorttext synchronized">Convierte los valores de la query a una tabla interna</p>
    METHODS convert_query_data_2_itab.
    "! <p class="shorttext synchronized">Número de ratios o figuras clave</p>
    "! Son las columnas que veríamos en la consulta de la RSRT
    "! @parameter rv_number | <p class="shorttext synchronized">Número de ratios</p>
    METHODS get_number_key_figure
      RETURNING
        VALUE(rv_number) TYPE i.
    "! <p class="shorttext synchronized">Número de características</p>
    "! Las características son las filas de datos de la consulta de la RSRT
    "! @parameter rv_number | <p class="shorttext synchronized">Número de características</p>
    METHODS get_number_charact
      RETURNING
        VALUE(rv_number) TYPE i.
    "! <p class="shorttext synchronized">Obtiene el detalle de las carácterísticas</p>
    "! Las características son las filas de datos de la consulta de la RSRT
    "! @parameter rt_detail | <p class="shorttext synchronized">Detalle</p>
    METHODS get_charact_detail
      RETURNING
        VALUE(rt_detail) TYPE bapi6108_t.
    "! <p class="shorttext synchronized">Construcción de la tabla interna para los datos</p>
    "! @parameter it_charact_detail | <p class="shorttext synchronized">Características</p>
    METHODS build_internal_table
      IMPORTING
        it_charact_detail TYPE bapi6108_t.
    "! <p class="shorttext synchronized">Limpieza de variables globales</p>
    METHODS clear_global_var.
    "! <p class="shorttext synchronized">Relleno de datos en la tabla interna</p>
    METHODS fill_itab_data.
    "! <p class="shorttext synchronized">Obtiene los datos de las filas</p>
    "! @parameter rt_rows | <p class="shorttext synchronized">Filas</p>
    METHODS get_rows_data
      RETURNING
        VALUE(rt_rows) TYPE rrws_tx_set.
    "! <p class="shorttext synchronized">Convierte el nombre de la característica</p>
    "! @parameter rv_chanm | <p class="shorttext synchronized">Nombre de la característica</p>
    METHODS convert_charact_name
      IMPORTING
        iv_chanm        TYPE rrws_sx_axis_chars-chanm
      RETURNING
        VALUE(rv_chanm) TYPE rrws_sx_axis_chars-chanm.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_bw_query_bex IMPLEMENTATION.
  METHOD constructor.
    mv_infoprovider = iv_infoprovider.
    mv_query = iv_query.
  ENDMETHOD.

  METHOD execute.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.
    CLEAR: et_fieldcat, eo_values, es_return.

    clear_global_var(  ). " Limpieza de variables globales

    " Si se pasan los parámetros se llama al método para guardarlos a nivel global.
    IF it_parameters IS SUPPLIED.
      set_parameters( it_parameters ).
    ENDIF.

    " Recuperamos los datos de la query bex
    get_query_data( IMPORTING es_return = es_return ).

    IF es_return IS INITIAL. " No hay errores.
      convert_query_data_2_itab(  ).

      et_fieldcat = mt_fieldcat.
      IF mo_itab_type IS BOUND.
        " Creo el objeto pasado por parámetro para poderle pasar los datos
*        CREATE DATA eo_values TYPE HANDLE mo_itab_type.
        eo_values = mo_itab_data.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD set_parameters.
    mt_parameters = it_parameters.
  ENDMETHOD.


  METHOD get_query_data.
    CLEAR: es_return.

    CALL FUNCTION 'RRW3_GET_QUERY_VIEW_DATA'
      EXPORTING
        i_infoprovider          = mv_infoprovider
        i_query                 = mv_query
*       i_view_id               =
        i_t_parameter           = mt_parameters
      IMPORTING
        e_axis_info             = mt_axis_info
        e_cell_data             = mt_cell_data
        e_axis_data             = mt_axis_data
      EXCEPTIONS
        no_applicable_data      = 1
        invalid_variable_values = 2
        no_authority            = 3
        abort                   = 4
        invalid_input           = 5
        invalid_view            = 6
        OTHERS                  = 7.
    IF sy-subrc NE 0.
      es_return-id = sy-msgid.
      es_return-type = sy-msgty.
      es_return-number = sy-msgno.
      es_return-message_v1 = sy-msgv1.
      es_return-message_v2 = sy-msgv2.
      es_return-message_v3 = sy-msgv3.
      es_return-message_v4 = sy-msgv4.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO es_return-message.
    ENDIF.

  ENDMETHOD.


  METHOD convert_query_data_2_itab.



    " Obtenemos el detalle de las características
    DATA lt_detail TYPE bapi6108_t.
    lt_detail = get_charact_detail(  ).

    IF lt_detail IS NOT INITIAL.
      build_internal_table( EXPORTING it_charact_detail = lt_detail ).

      " Si hay tabla interna global es momento de llenar.
      IF mo_itab_type IS BOUND.
        fill_itab_data(  ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_number_key_figure.
    CLEAR: rv_number.

    READ TABLE mt_axis_data ASSIGNING FIELD-SYMBOL(<ls_axis>) WITH KEY axis = '000'  .
    IF sy-subrc EQ 0 .
      LOOP AT <ls_axis>-set ASSIGNING FIELD-SYMBOL(<ls_set_dummy>)
                          GROUP BY ( tuple_ordinal = <ls_set_dummy>-tuple_ordinal )
                          ASSIGNING FIELD-SYMBOL(<group>).

        rv_number = rv_number + 1.

      ENDLOOP .
    ENDIF.
  ENDMETHOD.

  METHOD get_number_charact.
    CLEAR: rv_number.

    READ TABLE mt_axis_info  ASSIGNING FIELD-SYMBOL(<ls_axis>) WITH KEY axis = '001'  .
    IF sy-subrc EQ 0 .
      rv_number = lines( <ls_axis>-chars ) .
    ENDIF .
  ENDMETHOD.


  METHOD get_charact_detail.
    DATA lt_tmp_char TYPE rrws_thx_axis_chars .


    CLEAR rt_detail.

    " Se recorren las características para ponerla en una tabla temporal
    READ TABLE mt_axis_info ASSIGNING FIELD-SYMBOL(<ls_axis>) WITH KEY axis = '001'.
    IF sy-subrc EQ 0 .

      " Se recorren las características
      LOOP AT <ls_axis>-chars ASSIGNING FIELD-SYMBOL(<ls_chars>) .

        DATA(ls_tmp_char) = CORRESPONDING rrws_sx_axis_chars( <ls_chars> ).
        INSERT ls_tmp_char INTO TABLE lt_tmp_char.

        " Si tiene atributos se añaden
        LOOP AT <ls_chars>-attrinm ASSIGNING FIELD-SYMBOL(<ls_attrinm>) .
          CLEAR: ls_tmp_char-chanm.
          ls_tmp_char-chanm = <ls_attrinm>-attrinm.
          ls_tmp_char-caption = <ls_attrinm>-caption .
          INSERT ls_tmp_char INTO TABLE lt_tmp_char.
        ENDLOOP .

      ENDLOOP.

      " De cada característica se buscan el detalle para saber descripciones, etc..
      LOOP AT lt_tmp_char ASSIGNING <ls_chars>.

        " Convierto el nombre de la característica
        DATA(lv_new_charac) = convert_charact_name( <ls_chars>-chanm ).

        DATA(ls_iobj_detail) = VALUE bapi6108(  ).
        CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
          EXPORTING
            version    = rs_c_objvers-active
            infoobject = lv_new_charac
          IMPORTING
            details    = ls_iobj_detail.

        IF  NOT ls_iobj_detail IS INITIAL .
          INSERT ls_iobj_detail INTO TABLE rt_detail.
        ELSE.
          INSERT VALUE #( infoobject = lv_new_charac
                          textlong = <ls_chars>-caption ) INTO TABLE rt_detail.

        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD build_internal_table.

    DATA(lv_col_pos) = 1.

    " En el campo REF_FIELD pongo el campo original al cual apunta.
    " Esto servirá para poder localizar mejor que ratio queremos obtener. Este valor
    " Solo se pasa en el campo de valor, en el de unidad y moneda no se le pasa porque
    " se determinará en base al campo de valor.

    " Añadimos los campos de las características.
    LOOP AT it_charact_detail ASSIGNING FIELD-SYMBOL(<ls_detail>).
      DATA(ls_fieldcat) = VALUE lvc_s_fcat( fieldname = <ls_detail>-infoobject
                                            outputlen = '130'
                                            datatype = 'CHAR'
                                            scrtext_l = <ls_detail>-textlong
                                            col_pos = lv_col_pos
                                            ref_field = <ls_detail>-infoobject  ).

      IF ls_fieldcat-fieldname+0(1) EQ '0' .
        SHIFT ls_fieldcat-fieldname LEFT BY 1 PLACES .
      ENDIF .
      INSERT ls_fieldcat INTO TABLE mt_fieldcat.

    ENDLOOP .

    " Ahora añadimos los campos de los ratios
    DATA(lv_counter) = 1.
    READ TABLE mt_axis_data ASSIGNING FIELD-SYMBOL(<ls_axis>) WITH KEY axis = '000'  .
    IF sy-subrc EQ 0 .
      LOOP AT <ls_axis>-set ASSIGNING FIELD-SYMBOL(<ls_set_dummy>)
                            GROUP BY ( tuple_ordinal = <ls_set_dummy>-tuple_ordinal  )
                            ASSIGNING FIELD-SYMBOL(<group>).

        READ TABLE <ls_axis>-set ASSIGNING FIELD-SYMBOL(<ls_set>) WITH KEY tuple_ordinal = <group>-tuple_ordinal.
        IF sy-subrc = 0.
          lv_col_pos = lv_col_pos + 1.

          " Campo de valor. En el
          INSERT VALUE #( fieldname = |VALUE{ lv_counter }|
                          outputlen = 30
                          datatype = |CHAR|
                          scrtext_l = <ls_set>-caption
                          col_pos = lv_col_pos
                          ref_field = <ls_set>-chavl ) INTO TABLE mt_fieldcat.

          " Campo de moneda.
          INSERT VALUE #( fieldname = |VALUE{ lv_counter }{ cs_suffix_value_field-currency }|
                                    outputlen = 5
                                    datatype = |CHAR|
                                    scrtext_l = <ls_set>-caption
                                    col_pos = lv_col_pos ) INTO TABLE mt_fieldcat.

          " Campo de unidad.
          INSERT VALUE #( fieldname = |VALUE{ lv_counter }{ cs_suffix_value_field-unit }|
                                    outputlen = 5
                                    datatype = |CHAR|
                                    scrtext_l = <ls_set>-caption
                                    col_pos = lv_col_pos ) INTO TABLE mt_fieldcat.

          lv_counter = lv_counter + 1.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Quito posibles duplicados. Tengo en cuenta la posición
    SORT mt_fieldcat BY fieldname.
    DELETE ADJACENT DUPLICATES FROM mt_fieldcat COMPARING fieldname.
    " Vuelvo a poner los campos en el orden que toca.
    SORT mt_fieldcat BY col_pos.

    " Paso los campos al componente para poder crear la tabla.
    mt_itab_comp = VALUE #( FOR <wa> IN mt_fieldcat ( name = <wa>-fieldname
                                                      type = cl_abap_elemdescr=>get_c( CONV #( <wa>-outputlen ) ) ) ).


    " Si hay componentes, que debería, se crea la tabla dinámica
    IF mt_itab_comp IS NOT INITIAL.

      mo_struct_type =  cl_abap_structdescr=>create( p_components = mt_itab_comp
                                                     p_strict     = cl_abap_structdescr=>false ).

      CREATE DATA mo_struct_data TYPE HANDLE mo_struct_type.

      IF mo_struct_type IS BOUND.

        mo_itab_type =  cl_abap_tabledescr=>create( p_line_type  = mo_struct_type
                                                    p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA mo_itab_data TYPE HANDLE mo_itab_type.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD clear_global_var.

    CLEAR: mt_axis_data, mt_axis_info, mt_cell_data.
    CLEAR: mt_itab_comp, mt_fieldcat.
    CLEAR: mo_itab_type, mo_struct_type.

  ENDMETHOD.


  METHOD fill_itab_data.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.


    " Los datos que hay en MT_CELL_DATA van en bloque según en número de características  y los
    " ratios.
    " Ejemplo: estamos en un P&L donde tenemos 1 característica y 20 ratios. Y hay 5 filas con los datos
    " de las características. Los bloques son:
    " 1-21 -> Datos de la primera fila
    " 22-65 -> Datos de la segunda fila
    " 66-87 -> Datos de la tercera fila
    " etc..

    ASSIGN mo_itab_data->* TO <table>.
    ASSIGN mo_struct_data->* TO FIELD-SYMBOL(<line>).

    " Obtenemos el número de ratios o figuras clave
    DATA(no_of_keyf) = get_number_key_figure(  ).

    " Vamos a obtener las filas de la tabla. Estas filas salen del axis '001' que se puede
    " recorrer directamente pero hay que tener en cuenta sus atributos. Para no añadir complejidad
    " al proceso lo primero que hago es obtener las filas en un método aparte.
    DATA(lt_rows) = VALUE rrws_tx_set( ).
    lt_rows = get_rows_data(  ).


    DATA(lv_ini_block) = 0.

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<ls_rows_dummy>)
                    GROUP BY ( tuple_ordinal = <ls_rows_dummy>-tuple_ordinal )
                    ASSIGNING FIELD-SYMBOL(<group>).

      " A partie de la segunda vuelta resto uno al total para posicionar bien en el registro del inicio
      " del bloque
      IF lv_ini_block GT 0 .
        lv_ini_block = lv_ini_block - 1 .
      ENDIF .

      CLEAR <line> .

      READ TABLE lt_rows ASSIGNING FIELD-SYMBOL(<ls_rows>) WITH KEY tuple_ordinal = <group>-tuple_ordinal.

      " Convierto el nombre de la característica
      DATA(lv_new_charac) = convert_charact_name( <ls_rows>-chanm ).

      IF lv_new_charac+0(1) EQ '0' .
        SHIFT lv_new_charac LEFT BY 1 PLACES .
      ENDIF .

      " Asignamos a un puntero el campo de la característica
      ASSIGN COMPONENT lv_new_charac OF STRUCTURE <line> TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        IF <ls_rows>-chavl = '#' .
        ELSE .
          " Concateno el nombre externo y su descripción del valor de la fila
          <field> = |{ <ls_rows>-chavl_ext } - { <ls_rows>-caption }|.

          lv_ini_block = lv_ini_block + 1 .
          DATA(lv_counter_block) = 0.

          LOOP AT mt_cell_data ASSIGNING FIELD-SYMBOL(<ls_cell>) FROM lv_ini_block .

            " Cuenta el número de ratios informados. Si supera los leídos
            " se sale del proceso.
            lv_counter_block = lv_counter_block + 1 .
            IF lv_counter_block GT no_of_keyf .
              EXIT .
            ENDIF .

            " Campo de valor
            DATA(lv_fieldname) = |VALUE{ lv_counter_block }|.
            ASSIGN COMPONENT lv_fieldname OF STRUCTURE <line> TO <field>.
            IF sy-subrc = 0.
              <field> = <ls_cell>-value .
            ENDIF.

            " Campo de moneda
            lv_fieldname = |VALUE{ lv_counter_block }{ cs_suffix_value_field-currency }|.
            ASSIGN COMPONENT lv_fieldname OF STRUCTURE <line> TO <field>.
            IF sy-subrc = 0.
              <field> = <ls_cell>-currency .
            ENDIF.

            " Campo de unidad
            lv_fieldname = |VALUE{ lv_counter_block }{ cs_suffix_value_field-unit }|.
            ASSIGN COMPONENT lv_fieldname OF STRUCTURE <line> TO <field>.
            IF sy-subrc = 0.
              <field> = <ls_cell>-unit .
            ENDIF.

          ENDLOOP .

          " Se incrementa al contador global de bloque el número de ratios
          lv_ini_block = lv_ini_block + no_of_keyf .
          INSERT <line> INTO TABLE <table>.
        ENDIF.


      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_rows_data.

    CLEAR: rt_rows.

    " En el axis '001' tenemos las filas
    READ TABLE mt_axis_data ASSIGNING FIELD-SYMBOL(<ls_axis>) WITH KEY axis = '001' .
    IF sy-subrc = 0.
      " De los datos de las filas lo pasamos a la tabla añadiendole los atributos.
      LOOP AT <ls_axis>-set ASSIGNING FIELD-SYMBOL(<ls_set>).

        DATA(ls_tmp_set) = CORRESPONDING rrws_sx_tuple( <ls_set> ).
        INSERT ls_tmp_set INTO TABLE rt_rows.
        IF NOT <ls_set>-attributes[] IS INITIAL .
          LOOP AT <ls_set>-attributes INTO DATA(wa_dattrinm) .
            ls_tmp_set-chanm = wa_dattrinm-attrinm.
            ls_tmp_set-chavl = wa_dattrinm-attrivl.
            ls_tmp_set-chavl_ext = wa_dattrinm-attrivl.
            ls_tmp_set-caption = wa_dattrinm-caption .
            INSERT ls_tmp_set INTO TABLE rt_rows.
          ENDLOOP .
        ENDIF .
      ENDLOOP .
    ENDIF.

  ENDMETHOD.


  METHOD convert_charact_name.
    DATA off  TYPE i.
    DATA moff TYPE i.
    DATA mlen TYPE i.
    DATA lv_iobjnm TYPE rsd_iobjnm.

    DATA(lv_charac) = iv_chanm.

    FIND '___' IN SECTION OFFSET off OF lv_charac.
    IF sy-subrc EQ 0 .
      DATA(lv_iset_iobjnm) = CONV rsd_iobjnm( lv_charac ).
      CALL FUNCTION 'RSD_IOBJNM_GET_FROM_INFOSET'
        EXPORTING
          i_iset_iobjnm = lv_iset_iobjnm
        IMPORTING
          e_iobjnm      = lv_iobjnm
        EXCEPTIONS
          name_error    = 1
          no_field      = 2
          OTHERS        = 3.

      lv_charac = lv_iobjnm.

    ELSE.

      FIND '__' IN SECTION OFFSET off OF lv_charac MATCH OFFSET moff MATCH LENGTH mlen.
      IF sy-subrc EQ 0 .
        off = moff + mlen .
        SHIFT lv_charac LEFT BY off PLACES .
      ENDIF .
    ENDIF .

    rv_chanm = lv_charac.

  ENDMETHOD.

ENDCLASS.
