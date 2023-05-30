module Lib where
import Text.Show.Functions

laVerdad = True

-----------------------------------------Modelado Base--------------------------------------

type Idioma = String

data Turista = UnTurista{
    nivelEstres :: Float,
    nivelCansancio :: Float,
    viajaSolo:: Bool,
    idiomasTurista :: [Idioma]
}

type Excursion = Turista -> Turista --hace un cambio en el turista

cambiarEstresOCansancio :: Float -> Float -> Float
cambiarEstresOCansancio nivel suma = nivel + suma

irPlaya :: Excursion
irPlaya turista
    | viajaSolo turista =  hacerExcursion $ turista {nivelCansancio= cambiarEstresOCansancio (nivelCansancio turista) (-5)}
    | otherwise = hacerExcursion $ turista{nivelEstres= cambiarEstresOCansancio (nivelEstres turista) (-1)}

apreciarElementoPaisaje :: String -> Excursion
apreciarElementoPaisaje elemento turista = hacerExcursion $ turista {nivelEstres=  cambiarEstresOCansancio (nivelEstres turista) (fromIntegral $ length elemento*(-1))}

salirHablarIdioma :: Idioma -> Excursion
salirHablarIdioma idioma turista = hacerExcursion $ turista{idiomasTurista= (idiomasTurista turista) ++[idioma]}

salirCaminar :: Float -> Excursion
salirCaminar minutos turista = hacerExcursion $ turista{nivelCansancio=cambiarEstresOCansancio(nivelCansancio turista) (calcularIntensidad minutos), nivelEstres= cambiarEstresOCansancio(nivelEstres turista) (calcularIntensidad minutos * (-1))}

calcularIntensidad:: Float ->Float
calcularIntensidad minutos = fromIntegral $ round ( minutos/4)

paseoEnBarco :: String->Excursion
paseoEnBarco marea turista
    | marea=="fuerte"= hacerExcursion $ turista{nivelEstres= cambiarEstresOCansancio (nivelEstres turista) (6), nivelCansancio= cambiarEstresOCansancio (nivelCansancio turista)(10) }
    | marea=="tranquila" = hacerExcursion $ salirHablarIdioma "aleman" (apreciarElementoPaisaje "mar" (salirCaminar 10 turista))
    | otherwise = hacerExcursion turista

-----------------------------------------Punto 2--------------------------------------
ana:: Turista
ana = UnTurista 0 21 False ["espaniol"]

beto:: Turista
beto = UnTurista 15 15 True ["aleman"]

cathi:: Turista
cathi = UnTurista 15 15 True ["aleman","catalan"]

hacerExcursion:: Turista -> Turista
hacerExcursion turista= turista{nivelEstres= cambiarEstresOCansancio (nivelEstres  turista) (nivelEstres turista *0.1) }

-----------------------------------------Punto 3--------------------------------------
type Tour = [Excursion]

completo :: Tour
completo = [salirCaminar 20, apreciarElementoPaisaje "cascada",salirCaminar 2,salirHablarIdioma "Melmacquiano" ]

islaVecina :: String->Tour
islaVecina marea
    | marea=="fuerte"= [paseoEnBarco marea,  apreciarElementoPaisaje "lago", paseoEnBarco marea]
    | otherwise =[paseoEnBarco marea, irPlaya, paseoEnBarco marea]
    
ladoB :: Excursion->Tour
ladoB excursion= [paseoEnBarco "tranquilas"] ++ [excursion] ++ [salirCaminar 120]

tours :: String -> Excursion ->[Tour]
tours marea excursion = [completo, ladoB excursion, islaVecina marea]

hacerTour :: Turista -> Tour -> Turista
hacerTour turista [] = turista -- Si el tour está vacío, el turista no realiza ninguna excursión y se mantiene igual.
hacerTour turista (excursion:resto) = hacerTour (excursion turista{nivelEstres=nivelEstres turista+1}) resto  -- Se aplica la primera excursión al turista y se llama recursivamente con el resto del tour.

esDesestresante :: Excursion -> Turista -> Bool
esDesestresante excursion turista = nivelEstres (excursion turista) < nivelEstres turista

dejaAcompaniado :: Excursion -> Turista -> Bool
dejaAcompaniado excursion turista = nivelEstres (excursion turista) >= nivelEstres turista && viajaSolo (excursion turista) /= viajaSolo turista

excursionesConvincentes :: Tour -> Turista -> Bool
excursionesConvincentes [] _ = False -- Si el tour está vacío, no hay excursiones convincentes.
excursionesConvincentes (excursion:excursionesRestantes) turista = esDesestresante excursion turista && dejaAcompaniado excursion turista || excursionesConvincentes excursionesRestantes turista

tourConvincente :: Turista -> [Tour] -> Bool
tourConvincente _ [] = False -- Si la lista de tours está vacía, no hay ningún tour convincente.
tourConvincente turista (tour:toursRestantes) = excursionesConvincentes tour turista || tourConvincente turista toursRestantes

-----------------------------------------Punto 4--------------------------------------
visitaPlayas :: Tour
visitaPlayas = [irPlaya] ++ visitaPlayas

-- b 
-- para Ana se sabe que si es convincente ya que le va a bajar todo el estres y para  Beto no ya que viaja solo
-- c
-- 


