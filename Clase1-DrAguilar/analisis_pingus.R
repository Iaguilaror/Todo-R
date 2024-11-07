# Cargamos todos los paquetes del evento
source( "https://rebrand.ly/instalador2024" )

# Leemos la data de pinguinos Palmer
library( "palmerpenguins" )
pingu.df <- penguins

# Hagamos un resumen de las columnas
summary( pingu.df )

# Boxplot para comparar la masa corporal entre hembras y machos
boxplot( body_mass_g ~ sex, 
         data = pingu.df, 
         main = "Comparación de Masa Corporal entre Hembras y Machos", 
         xlab = "Sexo", 
         ylab = "Masa Corporal (g)", 
         col = c( "steelblue", "skyblue" ) )

# Define el diseño para colocar los histogramas uno debajo del otro
par( mfrow = c( 2, 1 ) )

# Definimos el rango común para el eje x
xlim_range <- range( pingu.df$body_mass_g, na.rm = TRUE )

# Histograma para hembras
hist( pingu.df$body_mass_g[ pingu.df$sex == "female" ], 
      main = "Masa Corporal de Pingüinas (Hembras)", 
      xlab = "Masa Corporal (g)", 
      col = "steelblue", 
      breaks = 15, 
      xlim = xlim_range )

# Histograma para machos
hist( pingu.df$body_mass_g[ pingu.df$sex == "male" ], 
      main = "Masa Corporal de Pingüinos (Machos)", 
      xlab = "Masa Corporal (g)", 
      col = "skyblue", 
      breaks = 15, 
      xlim = xlim_range )

# Restablece el diseño original
par( mfrow = c( 1, 1 ) )

# No olvides guardar tus plots con "EXPORT" en PNG

# FIN DE LA CLASE