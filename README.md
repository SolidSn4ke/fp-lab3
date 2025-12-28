# Лабораторная Работа 3

- Выполнил: Кузьмин Артемий Андреевич
- Группа: P3314

## Содержание

- [Ввод-Вывод](#ввод-вывод)
- [Команды](#команды)
  - [Виды команд](#виды-команд)
  - [Подписчики](#подписчики)
  - [Интерполяция](#интерполяция)
- [Тестирование](#тестирование)
- [Вывод](#вывод)

## Описание разработанного приложения

Это консольное приложение, способное расчитывать интерполяционнный многочлен методами лагранжа, ньютона и отрезками. Ниже подробно рассмотрены все основные компоненты приложения

### Ввод-Вывод

Ввод-вывод описан в файле `Main.hs`. Программа работает в бесконечном цикле пока не получит на выполнению команду `exit`.

```haskell
main :: IO ()
main = do
    putStrLn "Program is running. Enter \"help\" to see list of commands"
    inputLoop []
```

Основной цикл представляет из себя обработку ввода и вызов команды.

```haskell
inputLoop :: [Subscriber] -> IO ()
inputLoop subs = do
    inputPrompt
    input <- getLine
    resolve input
  where
    resolve str
        | str == "" = inputLoop subs
        | (2 == length (words str)) && ((length . filter (=~ "-?([1-9]\\d*[.,]\\d+|[1-9]\\d*|0[.,]\\d+|0)") $ words str) == 2) = do
            let x = read . head $ words str :: Double
            let y = read $ words str !! 1 :: Double
            mapM_ (\sub -> action sub (state sub) (x, y)) subs
            inputLoop subs
        | otherwise = do
            newSubs <- invoker (words str) subs
            inputLoop newSubs
```

Если введено два числа, то все подписчики уведомляются об этом событии и идет расчет интерполяции. Иначе ввод воспринимается как команда и вызывается

### Команды

Структура и виды команд описаны в файле `Commands.hs`

Структура данных для команд описана в record стиле и состоит из имени, действия и описания.

```haskell
data Command = Command {name :: String, execute :: [String] -> [Subscriber] -> IO [Subscriber], description :: String}
```

#### Виды команд

Всего доступно 6 команд:

- `exit` - завершение работы
- `help` - справка по командам
- `eof`- очистка массива подписчиков
- Три команды для интерполяции, по одной для каждого метода, будут рассмотрены подробно далее. С точки зрения кода, они просто добавляют нового подписчика в массив.

```haskell
listOfCommands :: [Command]
listOfCommands = [helpCommand, linearCommand, lagrangeCommand, newtonCommand, eofCommand, exitCommand]
  where
    helpCommand = Command{name = "help", execute = executeHelp, description = "help - see list of commands"}
    linearCommand = Command{name = "linear", execute = executeLinear, description = "linear <step> - linear interpolation with a specific step. Works in streaming mode. Enter \"eof\" to stop"}
    lagrangeCommand = Command{name = "lagrange", execute = executeLagrange, description = "lagrange <n> <step> - lagrange interpolation with n points and a specific step. Works in streaming mode. Enter \"eof\" to stop"}
    newtonCommand = Command{name = "newton", execute = executeNewton, description = "newton <n> <step> - newton interpolation with n points and a specific step. Works in streaming mode. Enter \"eof\" to stop"}
    eofCommand = Command{name = "eof", execute = executeEOF, description = "eof - stop streaming mode"}
    exitCommand = Command{name = "exit", execute = executeExit, description = "exit - close application"}
```

#### Подписчики

Тип данных для подписчиков описан в record стиле и состоит из имени, действия и состояния. Действие вызывается каждый раз, когда пользователь вводит точку. Состояние хранит все ранее введенные точки и последнюю точку, для которой был расчет интерполяции.

```haskell
data Subscriber = Subscriber {subName :: String, action :: IORef ([(Double, Double)], Double) -> (Double, Double) -> IO (), state :: IORef ([(Double, Double)], Double)}
```

#### Интерполяция

Рассмотри устройство команд для интерполяции на примере команды для метода Лагранжа. С точки зрения структуры, все эти команды выполняют одни и те же действия, отличие заключается лишь в методе интерполяции.

Сначала идет основная команда. Как уже было сказано ранее, основное действие в этом блоке - создание нового подписчика и добавление его в массив

```haskell
executeLagrange :: [String] -> [Subscriber] -> IO [Subscriber]
executeLagrange args subs =
    if length args < 3
        then putStrLn "< Too few args to execute" >> return subs
        else do
            let n = read (args !! 1) :: Int
            let step = read (args !! 2) :: Double
            st <- newIORef ([], 0)
            let sub =
                    Subscriber
                        { subName = "lagrange",
                          action = lagrangeAction n step,
                          state = st
                        }
            putStrLn "< Subscriber has been added"
            return (sub : subs)
```

Дальше в блоке `where` идет описание действия, которое следует выполнить для этого подписчика в случае получения новой точки. Конкретно для интерполяции многочленом Лагранжа это проверка, достаточно ли точек для расчета интерполяции и передача нужных точек в интерполяционыый метод.

```haskell
  where
    lagrangeAction :: Int -> Double -> IORef ([(Double, Double)], Double) -> (Double, Double) -> IO ()
    lagrangeAction n_ step_ stateRef point = do
        (points, start) <- readIORef stateRef
        resolve (length points) points start
      where
        resolve len points start
            | len == 0 = do modifyIORef stateRef (\(p, _) -> (point : p, fst point))
            | len < n_ = do modifyIORef stateRef (first (point :))
            | otherwise = do
                let prevs = take n_ points
                if fst point < start + step_
                    then return ()
                    else do
                        putStrLn $ "< lagrange: " ++ show (lagrange prevs (start + step_))
                        modifyIORef stateRef (\(p, s) -> (p, s + step_))
                        if fst point >= start + 2 * step_
                            then lagrangeAction n_ step_ stateRef point
                            else modifyIORef stateRef (first (point :))
```

Сам метод для интерполяции многочленом Лагранжа. Описан в файле `Interpolation.hs`, как и все остальные методы интерполяции.

```haskell
lagrange :: [(Double, Double)] -> Double -> Double
lagrange points x = foldl (\acc (xi, yi) -> acc + yi * l xi) 0 points
  where
    l xj = foldl (\acc (xi, _) -> if xi == xj then acc else acc * (x - xi) / (xj - xi)) 1 points
```

### Тестирование

Включает лишь модульное тестирование методов интерполяции из `Interpolation.hs`. Реализовано в файле `Spec.hs`.

## Вывод

В ходе работы я узнал как происходит работа с вводом-выводм в haskell, в частности как работать с монадой IO, а также как работать с состоянием на примере IORef. Помимо этого, я также попробовал реализовать шаблон проектирования команда в рамках функционального языка программирования. Результатом работы стало разработаное консольное приложение
