# ABAP_BW_UTILS

En este repositorio se aglutirán las utilidades que vaya haciendo para BW. 

La recopilación de objetos es la siguiente:

# Clase ZCL_CA_BW_QUERY_BEX

Esta clase obtiene los datos de una Query Bex. Estas querys se generán con el Query Designer y pueden ser utilizidas por vía oData. Esta clase es mejorable pero es un punto de partida para una necesidad que tenía en un cliente.

Los métodos principales son:

* CONSTRUCTOR -> Se le pasa el nombre del infoprovider y nombre de la query. Estos valores se obtiene de las propiedas generales de la query en el query designer.
* SET_PARAMETER -> Permite pasarse las variables de la query. Este método esta hecho para poder pasar los valores y luego ejecutar la query. Hasta ahora tengo identificadas tres tipos de variable que son:

1. Variable que permite multiples valores, que no rangos, se informarían asi
                               ( name = 'VAR_NAME_1' value = '<nombre variable>' )
                               ( name = 'VAR_VALUE_EXT_1' value = '<valor>' )
2. Variable que permite rangos:                               
                               ( name = 'VAR_NAME_2' value = '<nombre variable>' )
                               ( name = 'VAR_SIGN_2' value = 'I' )
                               ( name = 'VAR_OPERATOR_2' value = 'EQ' )
                               ( name = 'VAR_VALUE_LOW_EXT_2' value = '<valor>' )
3. Variable que permite un solo valor:                                 
                               ( name = 'VAR_NAME_3' value = '<nombre variable>' )
                               ( name = 'VAR_VALUE_EXT_3' value = '<valor>' )
  
  Aquí el truco es que el número final: _1, _2, _3, etc, en el nombre de la variable se usa como identificador de la variable.
  
  * EXECUTE -> Ejecuta la query y devuelve los datos. A este método se le puede pasar opcionalmente las variables que ejecutará la query, si no se le pasa se tomará las variables pasadas en el parámetro anterior. Este método devuelve:
  
    * EO_VALUES -> Objeto con los valores de la query.
    * ET_FIELDCAT -> Catalogo de campos que se uso para montar la tabla interna dinámica para montar los valores
    * ES_RETURN -> Posible error devuelto al llamar a la query.

En la tabla de datos los campos que se devuelven son:
  
* Campos con que se definen en el apartado "Líneas" de la query
* Campos que se definen en el apartado "Columnas", que tendrá los ratios definidos (u otras columnas, pero en mi caso solo hay ratios). Estos campos siempre tienen el prefijo "VALUE" y un contador. Además se añade dos campos que será: "VALUE<contador>_CURRENCY" y "VALUE<contador>_UNIT" devolviendo la moneda y unidad, respectivamente, que estará informado en los ratios. Para saber a que ratio pertenece cada campo se puede hacer de dos maneras: 1) Los campos de valor se ponen en el mismo orden que esta definido en la columnas de la query 2) Consultando la tabla ET_FIELDCAT.
* ET_FIELDCAT -> Catalogo de campos que se usa para montar la tabla interna dinamica. Los campos que se rellenan son:
  * FIELDNAME -> Nombre del campo en EO_VALUES
  * SCRTEXT_L -> Texto del campo en la query
  * REF_FIELD -> Para los campos de valores, o ratios, contiene el UID del ratio, o columna. Este UID lo podemos ver en las propiedades del campo de la columna. Con lo cual saber el valor sería buscar el campo por el UID del ratio y con un puntero accedemos al valor en la tabla de datos. Es así como lo uso.
