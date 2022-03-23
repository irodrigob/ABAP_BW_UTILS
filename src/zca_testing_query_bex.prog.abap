*&---------------------------------------------------------------------*
*& Report zfrt_testing_query_bex
*&---------------------------------------------------------------------*
*& Programa para testear la clase que obtiene los datos de la query Bex
*& Para no complicarme la vida las variables se han de poner en el propio
*& código fuente.
*&---------------------------------------------------------------------*
REPORT zca_testing_query_bex.

FIELD-SYMBOLS: <tbl> TYPE STANDARD TABLE.

PARAMETERS: p_info  TYPE    rsinfoprov DEFAULT '2CZFRT_CUBE_PL_V01',
            p_query TYPE    rszcompid DEFAULT 'ZFRT_CUBE_PL01_Q01'.


START-OF-SELECTION.

  DATA(lo_query) = NEW zcl_ca_bw_query_bex( iv_infoprovider = p_info
                                            iv_query = p_query ).

  lo_query->set_parameters( it_parameters = VALUE #(
                               " Variable que permite multiples valores, que no rangos.
                               ( name = 'VAR_NAME_1' value = 'ZFRT_VAR_REPCOMP_MULTI_MAND' )
                               ( name = 'VAR_VALUE_EXT_1' value = 'ES45' )
                               " Variable que permite rangos
                               ( name = 'VAR_NAME_2' value = 'ZFRT_VAR_PERIOD_RANG' )
                               ( name = 'VAR_SIGN_2' value = 'I' )
                               ( name = 'VAR_OPERATOR_2' value = 'EQ' )
                               ( name = 'VAR_VALUE_LOW_EXT_2' value = '1' )
                               " Variable que permite un solo valor
                               ( name = 'VAR_NAME_3' value = 'ZFRT_VAR_YEAR_MAND' )
                               ( name = 'VAR_VALUE_EXT_3' value = '2021' )
                                )  ).

  lo_query->execute( IMPORTING eo_values    = DATA(lo_values)
                               et_fieldcat  = DATA(lt_fieldcat)
                               es_return    = DATA(ls_return) ).

  ASSIGN lo_values->* TO <tbl>.

  cl_demo_output=>display_data( <tbl> ).
