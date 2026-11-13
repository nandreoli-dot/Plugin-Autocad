¿Qué hace el plugin?
El comando principal REDET simplifica un flujo de trabajo que normalmente requiere múltiples pasos manuales.

Sus funciones principales son:

Extracción basada en contornos: Permite seleccionar un círculo o una polilínea cerrada existente (o dibujar un círculo al vuelo) para definir el área del detalle que se quiere ampliar. Aislamiento y Copia de Elementos: Identifica y copia automáticamente todos los objetos que se encuentran dentro del contorno seleccionado sin afectar el modelo original.

Escalado Automático: Solicita un factor de ampliación o una escala específica y agranda los elementos copiados según la configuración del usuario.   Recorte Inteligente (XCLIP): Convierte los elementos copiados en un bloque y le aplica una máscara de recorte (XCLIP) con la misma forma del contorno original, asegurando que solo se vea lo que estaba dentro del límite.

Etiquetado y Marco Automático: Genera un marco delimitador escalado alrededor del detalle y añade un texto (MTEXT) en la parte inferior con la identificación y la escala (por ejemplo: "DETALLE A - ESCALA 1:25").

Empaquetado en Bloques Limpios: Agrupa el contenido recortado, el marco y el texto del título dentro de un único bloque final consolidado (nombrado automáticamente como DETALLE-<nombre>) para facilitar su manipulación.

Gestión de Capas: Crea y asigna los elementos generados a una capa específica llamada ARQ-DETALLES por defecto, asignándole el color blanco.

¿Cómo usarlo?
Carga el archivo en AutoCAD arrastrando redet.lsp a la ventana de dibujo o usando el comando APPLOAD.
Escribe el comando REDET en la barra de comandos.
Selecciona el contorno (círculo o polilínea cerrada) o presiona Enter para dibujar uno nuevo.
El programa te pedirá: Una letra o número para identificar el detalle.
El factor de escala o ampliación.
El título del detalle.
La altura del texto.
El punto de destino donde deseas insertar el detalle ampliado.