---
layout: default
---

# Guía de uso de beatviz

[English version](../)

beatviz escucha la música con el micrófono del teléfono, encuentra el pulso e ilumina una barra en cada tiempo. Te muestra dónde está el pulso, a qué velocidad va (en pulsaciones por minuto, BPM) y dónde empieza cada compás.

Si eres músico y tocas con otros músicos o sobre grabaciones, puede serte útil tener un refuerzo visual del ritmo para no perder la cuenta de en qué parte del compás estás.

También puede servir con fines educativos a músicos, bailarines y a cualquiera que esté aprendiendo a oír el pulso: contar un vals, encontrar el "uno" de una canción o comprobar el tempo de un tema.

## Primeros pasos

1. Abre beatviz y permite el acceso al micrófono cuando te lo pida. beatviz necesita el micrófono para oír la música.
2. Pon música, en un altavoz u otro dispositivo, lo bastante alta para que destaque claramente sobre el ruido de fondo.
3. Si la música no es de 4/4 (la mayoría de la música popular lo es), ajusta el número de tiempos por compás con los botones **−** y **+**.
4. Pulsa **INICIAR**.
5. Tras unos pocos compases las barras empiezan a seguir la música y aparece el tempo, por ejemplo **120 BPM**. Dale unos 10 segundos para que se estabilice en un tempo preciso.

Pulsa **PARAR** cuando termines. beatviz también deja de escuchar cuando sales de la aplicación.

## La pantalla

De arriba abajo:

- **Las barras.** En cada tiempo se enciende una barra, de izquierda a derecha, y luego vuelve a empezar. La barra encendida muestra su número, para que puedas contar a la vez.
- **El indicador de BPM.** El tempo de la música. Muestra `--- BPM` hasta que beatviz encuentra el pulso. A su izquierda está el chip **1** (ver [Pulso inicial](#tiempo-1)); a su derecha aparece un candado cuando el tempo está bloqueado (ver [TAP](#tap)).
- **Tiempos por compás:** los botones **−** y **+**.
- **INICIAR / PARAR** y **TAP.**
- **?**: vuelve a mostrar las diapositivas de introducción. Arriba a la izquierda puedes elegir el idioma: **Auto** (el del teléfono), **EN** (inglés) o **ES** (español).

## Cómo sigue beatviz la música

beatviz predice cada tiempo un instante antes de que llegue, así que la barra se enciende en el tiempo y no después. Al principio necesita unos pocos tiempos para engancharse, y después sigue la música de forma estable. Los errores breves, como un tiempo perdido o un redoble de batería, no le hacen perderse.

Cuando la música baja de volumen (un corte, una introducción suave), el indicador de BPM se atenúa. beatviz mantiene entonces las barras al mismo tempo y espera a que vuelva la música, en lugar de intentar adivinar a partir del ruido.

Si el tempo de la canción cambia, beatviz lo sigue poco a poco a lo largo de unos cuantos tiempos.

## Ayudar a beatviz con TAP {#tap}

A veces beatviz sigue un pulso equivocado: el doble de rápido o la mitad de lo que tú contarías, o a contratiempo. Puedes enseñarle el pulso:

1. Pulsa **TAP** unas cuantas veces al ritmo de la música, en cada tiempo.
2. Las barras saltan a tu pulso y aparece un **candado** junto al BPM.

Mientras el tempo está bloqueado, beatviz mantiene tu tempo y tu pulso, y solo hace pequeñas correcciones a partir de lo que oye.

Para quitar el bloqueo:

- pulsa **TAP** una sola vez (tras una pausa de un par de segundos), o
- toca el indicador de BPM o el candado.

beatviz vuelve entonces a seguir la música por sí solo. TAP solo funciona mientras beatviz está escuchando.

## Pulso inicial {#tiempo-1}

La música se organiza en compases de unos pocos tiempos cada uno, y el primer tiempo de cada compás, el "uno", suele ser el más fuerte. beatviz intenta encontrarlo por sí solo, para que la barra 1 se encienda en el uno. Esto tarda más que encontrar el tempo: a menudo otros 10 segundos más o menos después de que el tempo se haya estabilizado.

El chip **1** junto al BPM indica lo que está pasando:

| Chip | Significado |
|---|---|
| **1** gris con borde | beatviz todavía está buscando el pulso inicial. |
| **1** blanco sobre verde | beatviz ha encontrado el pulso inicial. |
| **1** oscuro sobre naranja | Has elegido tú el pulso inicial. |

Para elegir el pulso initial, toca una barra en el momento en que suena su tiempo. Por ejemplo, toca la primera barra justo cuando oigas el primer tiempo de un compás. A partir de ese momento, ese tiempo es la barra 1 y el chip se pone naranja.

Para volver al modo automático, pulsa el chip **1** naranja.

Encontrar pulso inicial es más difícil que encontrar el tempo, y beatviz se equivoca más a menudo, sobre todo en música sin un tiempo fuerte claro. Por eso siempre puedes elegirlo tú.

## Tiempos por compás

Usa **−** y **+** para elegir cuántas barras se muestran, que es el número de tiempos de cada compás de la música:

- **4** para la mayoría del pop, rock, jazz y música de baile;
- **3** para los valses;
- otros valores para otros compases, o **1** para ver solo el pulso.

beatviz solo encuentra pulso inicial automáticamente cuando este número coincide con la música.

## Versión gratuita y Pro

La versión gratuita tiene todas las funciones de beatviz. Su único límite es un minuto de escucha cada vez que pulsas INICIAR; pulsa INICIAR de nuevo para continuar.

beatviz Pro permite escuchar sin límite.

Pulsa **Pasar a Pro** para ver las opciones de Pro. Si ya compraste Pro, por ejemplo en otro teléfono o antes de reinstalar la aplicación, pulsa **Restaurar** para recuperarlo.

## Consejos para un mejor seguimiento

- **Pon la música lo bastante alta.** beatviz funciona mejor cuando la música suena claramente más fuerte que las voces u otros ruidos de la sala.
- **Usa un micrófono externo.** Un micrófono USB, conectado directamente o con un adaptador USB, consigue un seguimiento bastante más estable que el micrófono del teléfono. Colócalo cerca de los altavoces. Los auriculares Bluetooth suelen añadir retardo y empeorar la calidad del sonido, así que es mejor uno con cable.
- **Mantén el teléfono quieto**, lejos de cosas que vibren, y no tapes el micrófono.
- **La música con un pulso claro funciona mejor.** El baile, el pop y el rock son fáciles; la música clásica con rubato, el free jazz o la música ambiental pueden ser difíciles de seguir, incluso para las personas.
- **Si las barras siguen un pulso equivocado**, usa TAP.

## Cómo funciona

Para los curiosos, esto es lo que pasa entre el micrófono y las barras. Todo se ejecuta en el teléfono.

**1. Del sonido a las características.** El sonido del micrófono se convierte a 22 050 muestras por segundo. Cada 20 ms (50 veces por segundo), beatviz mira los últimos 80 ms de sonido y mide su intensidad en 144 bandas de frecuencia, desde los graves más profundos hasta los platillos agudos. También mide cuánto acaba de subir cada banda, lo que resalta los golpes de batería y los ataques de las notas. El resultado es una lista de 288 números por cada trama de 20 ms.

**2. Una red neuronal.** Estos números pasan por una red neuronal ([BeatNet+](#creditos)) entrenada con muchas grabaciones en las que alguien marcó cada tiempo. Para cada trama da tres probabilidades que suman 1: que la trama sea un tiempo, que sea el primer tiempo de un compás, o ninguna de las dos. La red tiene memoria (capas LSTM), así que cada respuesta tiene en cuenta el ritmo reciente, pero solo usa sonido que ya se ha oído, y eso es lo que le permite funcionar en tiempo real.

**3. Un filtro de partículas.** Las probabilidades tienen ruido, así que no deciden el pulso por sí solas. Un filtro de partículas mantiene muchas hipótesis a la vez, cada una del tipo "el tempo es X y estamos en este punto entre dos tiempos". En cada trama, cada hipótesis avanza en el tiempo; las que esperan un tiempo cuando la red dice "tiempo" se vuelven más probables, y las que no, menos probables, y se sustituyen por copias de las mejores. Cuando las hipótesis que quedan coinciden en que ha llegado un tiempo, se notifica un tiempo, con el tempo actual. Un segundo conjunto de hipótesis averigua en qué lugar del compás cae cada tiempo, para compases de 2, 3 o 4 tiempos, y marca los tiempos que empiezan un compás.

**4. La pantalla.** La aplicación convierte estos tiempos en una rejilla regular de tiempos predichos. La rejilla sigue las detecciones poco a poco, así que una sola detección errónea no la mueve; sigue adelante en los pasajes silenciosos, y se muestra un poco adelantada para compensar el instante que se tarda en oír y analizar cada tiempo. El pulso inicial se sitúa comparando los primeros tiempos de compás detectados con tu ajuste de tiempos por compás, salvo que lo hayas elegido tú.

## Privacidad

beatviz analiza el sonido en tu teléfono, mientras escucha. El audio no se graba, no se guarda y no se envía a ningún sitio.

Las compras las gestionan Google Play y RevenueCat, que reciben la información de la compra y un identificador anónimo de la aplicación, nunca tu audio. Consulta la [política de privacidad](../privacy/) completa (en inglés).

## Créditos {#creditos}

La detección del pulso usa [BeatNet+](https://github.com/Soundbrenner/BeatNet-Plus), de Mojtaba Heydari, Frank Cwitkowitz y Zhiyao Duan, con licencia [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). beatviz ejecuta en el teléfono una versión en C++ de su modelo.
