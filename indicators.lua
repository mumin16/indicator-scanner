-- Global Metadata & Constants
DISCARD_RETURN     = -1
OK_RETURN          = 0

DRAW_NONE          = 0
DRAW_POINT         = 0
DRAW_HISTOGRAM     = 1
DRAW_LINE          = 2
DRAW_SECTION       = 3
DRAW_PAIRHISTOGRAM = 4
DRAW_TRIANGLESTRIP = 5
DRAW_DASHEDLINE    = 6
DRAW_LEVELS        = 7
DRAW_TRENDLINE=8
DRAW_HARMONIC=9

SHIFT_NONE         = 0

-- APPLIED PRICE CONSTANTS
PRICE_CLOSE        = 0
PRICE_OPEN         = 1
PRICE_HIGH         = 2
PRICE_LOW          = 3
PRICE_MEDIAN       = 4
PRICE_TYPICAL      = 5
PRICE_WEIGHTED     = 6

-- MA METHOD CONSTANTS
MODE_SMA           = 0
MODE_EMA           = 1
MODE_SMMA          = 2
MODE_LWMA          = 3

-- COLOR CONSTANTS
CLR_NONE           = 0x000000
CLR_RED            = 0xE6194B
CLR_GREEN          = 0x3CB44B
CLR_YELLOW         = 0xFFE119
CLR_BLUE           = 0x4363D8
CLR_ORANGE         = 0xF58231
CLR_PURPLE         = 0x911EB4
CLR_CYAN           = 0x42D4F4
CLR_MAGENTA        = 0xF032E6
CLR_LIME           = 0xBFEF45
CLR_PINK           = 0xFABEBE
CLR_TEAL           = 0x469990
CLR_LAVENDER       = 0xE6BEFF
CLR_BROWN          = 0x9A6324
CLR_BEIGE          = 0xFFFAC8
CLR_MAROON         = 0x800000
CLR_MINT           = 0xAAFFC3
CLR_OLIVE          = 0x808000
CLR_APRICOT        = 0xFFD8B1
CLR_NAVY           = 0x000075
CLR_GREY           = 0xA9A9A9
CLR_WHITE          = 0xFFFFFF
CLR_BLACK          = 0x000000

-- Global Price Data Buffers
DATE               = {}
TIME               = {}
OPEN               = {}
HIGH               = {}
LOW                = {}
CLOSE              = {}
VOLUME             = {}
MEDIAN             = {}
TYPICAL            = {}
WEIGHTED           = {}


-- Local Speed Optimizations (Math Upvalues)
local math_max   = math.max
local math_min   = math.min
local math_abs   = math.abs
local math_sqrt  = math.sqrt
local math_log   = math.log
local math_exp   = math.exp
local math_floor = math.floor
local math_atan  = math.atan


--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
--------------------------------------------------------------------------------
local function resolve_source(source)
  if type(source) == "number" then
    if source == 0 then
      return CLOSE
    elseif source == 1 then
      return OPEN
    elseif source == 2 then
      return HIGH
    elseif source == 3 then
      return LOW
    elseif source == 4 then
      return MEDIAN
    elseif source == 5 then
      return TYPICAL
    elseif source == 6 then
      return WEIGHTED
    end
  end
  return source
end

--------------------------------------------------------------------------------
-- MATHEMATICAL / STATISTICAL UTILITIES
--------------------------------------------------------------------------------
function HIGHEST(source, period)
  source = resolve_source(source)
  local n = #source
  local out = {}
  for i = 1, n do
    local max_val = source[i]
    local start_j = math_max(1, i - period + 1)
    for j = start_j, i - 1 do
      if source[j] > max_val then max_val = source[j] end
    end
    out[i] = max_val
  end
  return out
end

function LOWEST(source, period)
  source = resolve_source(source)
  local n = #source
  local out = {}
  for i = 1, n do
    local min_val = source[i]
    local start_j = math_max(1, i - period + 1)
    for j = start_j, i - 1 do
      if source[j] < min_val then min_val = source[j] end
    end
    out[i] = min_val
  end
  return out
end

-- Sliding window sum O(N)
function SUM(source, period)
  source = resolve_source(source)
  local n = #source
  local out = {}
  if n < period then return out end

  local running_sum = 0
  for i = 1, period do running_sum = running_sum + source[i] end
  out[1] = running_sum

  for i = 2, n - period + 1 do
    running_sum = running_sum - source[i - 1] + source[i + period - 1]
    out[i] = running_sum
  end
  return out
end

--------------------------------------------------------------------------------
-- MOVING AVERAGES & INDICATORS
--------------------------------------------------------------------------------
function SMA(source, period)
  source = resolve_source(source)
  local sum_arr = SUM(source, period)
  local out = {}
  local inv_period = 1 / period
  for j = 1, #sum_arr do out[j] = sum_arr[j] * inv_period end
  return out
end

function LWMA(source, period)
  source = resolve_source(source)
  local n = #source
  local out = {}
  local div = period * (period + 1) * 0.5

  for i = 1, n - period + 1 do
    local sum = 0
    for j = 0, period - 1 do
      sum = sum + (source[i + j] * (j + 1))
    end
    out[i] = sum / div
  end
  return out
end

function EMA(source, period)
  source = resolve_source(source)
  local n = #source
  local out = {}
  if n == 0 then return out end

  local multiplier = 2 / (period + 1)
  out[1] = source[1]
  for i = 2, n do
    out[i] = (source[i] - out[i - 1]) * multiplier + out[i - 1]
  end
  return out
end

function SMMA(source, period)
  source = resolve_source(source)
  local n = #source
  local out = {}
  if n < period then return out end

  local sma1 = 0
  for j = 1, period do sma1 = sma1 + source[j] end

  out[1] = sma1 / period
  for i = 2, n - period + 1 do
    out[i] = (out[i - 1] * (period - 1) + source[i + period - 1]) / period
  end
  return out
end

function HMA(source, period)
  source = resolve_source(source)
  local n = #source
  local p = math_floor(math_sqrt(period))
  local medp = math_floor(period * 0.5)

  local function FastLWMA(pos, len, price)
    if pos < len or len <= 0 then return 0.0 end
    local sum, wsum = 0.0, 0
    for i = len, 1, -1 do
      wsum = wsum + i
      sum = sum + price[pos - i + 1] * (len - i + 1)
    end
    return sum / wsum
  end

  local HMABuffer = {}
  for i = period, n do
    HMABuffer[i - period + 1] = 2 * FastLWMA(i, medp, source) - FastLWMA(i, period, source)
  end

  local out = LWMA(HMABuffer, p)
  local colorbuffer = { [1] = CLR_GREEN }
  for i = 2, #out do
    colorbuffer[i] = (out[i] > out[i - 1]) and CLR_GREEN or CLR_RED
  end
  return out, colorbuffer
end

function STDDEV(source, period, matype)
  source = resolve_source(source)
  local sma = (matype == 1 and EMA(source, period)) or
      (matype == 2 and SMMA(source, period)) or
      (matype == 3 and LWMA(source, period)) or
      SMA(source, period)
  local out = {}
  local inv_period = 1 / period

  for i = 1, #sma do
    local sum = 0
    local offset = i + period - 1
    for j = 0, period - 1 do
      local diff = source[offset - j] - sma[i]
      sum = sum + diff * diff
    end
    out[i] = math_sqrt(sum * inv_period)
  end
  return out
end

function BollingerBands(source, period, deviation)
  source = resolve_source(source)
  local sma = SMA(source, period)
  local upper, lower = {}, {}
  local inv_period = 1 / period

  for i = 1, #sma do
    local sum = 0
    local offset = i + period - 1
    for j = 0, period - 1 do
      local diff = source[offset - j] - sma[i]
      sum = sum + diff * diff
    end
    local dev = math_sqrt(sum * inv_period) * deviation
    upper[i] = sma[i] + dev
    lower[i] = sma[i] - dev
  end
  return sma, upper, lower
end

--------------------------------------------------------------------------------
-- OSCILLATORS & INDICATORS
--------------------------------------------------------------------------------
function BEARSPOWER(period)
  local out = {}
  local emabuffer = EMA(CLOSE, period)
  for i = 1, #CLOSE do out[i] = LOW[i] - emabuffer[i] end
  return out
end

function BULLSPOWER(period)
  local out = {}
  local emabuffer = EMA(CLOSE, period)
  for i = 1, #CLOSE do out[i] = HIGH[i] - emabuffer[i] end
  return out
end

function MOMENTUM(source, period)
  source = resolve_source(source)
  local out = {}
  local n = #source
  for i = 1, n - period do
    out[i] = (source[i + period] / source[i]) * 100
  end
  return out
end

function DEMARKER(period)
  local n = #HIGH
  local DeMin, DeMax = {}, {}
  DeMax[1], DeMin[1] = 0, 0

  for i = 2, n do
    DeMax[i] = (HIGH[i] > HIGH[i - 1]) and (HIGH[i] - HIGH[i - 1]) or 0
    DeMin[i] = (LOW[i] < LOW[i - 1]) and (LOW[i - 1] - LOW[i]) or 0
  end

  local demaxsma = SMA(DeMax, period)
  local deminsma = SMA(DeMin, period)
  local out = {}
  for i = 1, n - period + 1 do
    local denom = demaxsma[i] + deminsma[i]
    out[i] = (denom ~= 0) and (demaxsma[i] / denom) or 0
  end
  return out
end

function FORCE(source, period, matype)
  source = resolve_source(source)
  local sma = (matype == 1 and EMA(source, period)) or
      (matype == 2 and SMMA(source, period)) or
      (matype == 3 and LWMA(source, period)) or
      SMA(source, period)
  local out = {}
  out[1] = VOLUME[period] * sma[1]
  for i = 2, #source - period + 1 do
    out[i - 1] = VOLUME[i + period - 1] * (sma[i] - sma[i - 1])
  end
  return out
end

function OBV(source)
  source = resolve_source(source)
  local n = #source
  local out = {}
  if n == 0 then return out end

  out[1] = VOLUME[1]
  for i = 2, n do
    if source[i] > source[i - 1] then
      out[i] = out[i - 1] + VOLUME[i]
    elseif source[i] < source[i - 1] then
      out[i] = out[i - 1] - VOLUME[i]
    else
      out[i] = out[i - 1]
    end
  end
  return out
end

function ATR(period)
  local n = #CLOSE
  local tr = {}
  if n == 0 then return tr end

  tr[1] = HIGH[1] - LOW[1]
  for i = 2, n do
    local hl = HIGH[i] - LOW[i]
    local hc = math_abs(CLOSE[i - 1] - HIGH[i])
    local lc = math_abs(CLOSE[i - 1] - LOW[i])
    tr[i] = math_max(hl, hc, lc)
  end
  return SMA(tr, period)
end

function CCI(source, period)
  source = resolve_source(source)
  local sma = SMA(source, period)
  local mean = {}

  for i = 1, #sma do
    local sum = 0
    for j = 0, period - 1 do
      sum = sum + math_abs(source[i + j] - sma[i])
    end
    mean[i] = sum / period
  end

  local out = {}
  for k = 1, #sma do
    local div = 0.015 * mean[k]
    out[k] = (div ~= 0) and ((source[k + period - 1] - sma[k]) / div) or 0
  end
  return out
end

function MFI(period)
  local n = #TYPICAL
  local pmf, nmf = {}, {}
  if n == 0 then return {} end

  pmf[1], nmf[1] = TYPICAL[1] * VOLUME[1], 0
  for i = 2, n do
    local tp = TYPICAL[i] * VOLUME[i]
    if TYPICAL[i] > TYPICAL[i - 1] then
      pmf[i], nmf[i] = tp, 0
    elseif TYPICAL[i] < TYPICAL[i - 1] then
      pmf[i], nmf[i] = 0, tp
    else
      pmf[i], nmf[i] = 0, 0
    end
  end

  local spmf = SUM(pmf, period)
  local snmf = SUM(nmf, period)
  local out = {}
  for k = 1, #spmf do
    out[k] = 100 - (100 / (1 + (spmf[k] / snmf[k])))
  end
  return out
end

--Relative Strength Index
function RSI(source, period)
  source = resolve_source(source)

  local g = {}
  local l = {}
  local out = {}
  if period > #source then return out end

  for i = 2, #source, 1 do
    if source[i] > source[i - 1] then
      g[i - 1] = source[i] - source[i - 1]
      l[i - 1] = 0
    end
    if source[i] < source[i - 1] then
      l[i - 1] = source[i - 1] - source[i]
      g[i - 1] = 0
    end
    if source[i] == source[i - 1] then
      l[i - 1] = 0
      g[i - 1] = 0
    end
  end
  local ag = { 0 }
  local al = { 0 }

  for j = 1, period, 1 do
    ag[1] = ag[1] + g[j]
    al[1] = al[1] + l[j]
  end
  ag[1] = ag[1] / period
  al[1] = al[1] / period

  for k = 2, #g - period + 1, 1 do
    ag[k] = (ag[k - 1] * (period - 1) + g[k + period - 1]) / period
    al[k] = (al[k - 1] * (period - 1) + l[k + period - 1]) / period
  end

  for m = 1, period, 1 do
    out[m] = 100 - (100 / (1 + (ag[1] / al[1])))
  end

  for m = 1, #ag, 1 do
    out[m + period] = 100 - (100 / (1 + (ag[m] / al[m])))
  end
  return out
end

function RVI(period)
  local n = #CLOSE
  local MovAverage, RangeAverage = {}, {}
  for i = 4, n do
    MovAverage[i - 3]   = ((CLOSE[i] - OPEN[i]) + 2 * (CLOSE[i - 1] - OPEN[i - 1]) + 2 * (CLOSE[i - 2] - OPEN[i - 2]) + (CLOSE[i - 3] - OPEN[i - 3])) /
        6
    RangeAverage[i - 3] = ((HIGH[i] - LOW[i]) + 2 * (HIGH[i - 1] - LOW[i - 1]) + 2 * (HIGH[i - 2] - LOW[i - 2]) + (HIGH[i - 3] - LOW[i - 3])) /
        6
  end

  local NUM = SUM(MovAverage, period)
  local DENUM = SUM(RangeAverage, period)
  local out = {}
  for j = 1, #NUM do
    out[j] = (DENUM[j] ~= 0) and (NUM[j] / DENUM[j]) or 0
  end
  return out
end

function RVISIGNAL(period)
  local ExtRVIBuffer = RVI(period)
  local ExtSignalBuffer = {}
  for i = 4, #ExtRVIBuffer do
    ExtSignalBuffer[i - 3] = (ExtRVIBuffer[i] + 2 * ExtRVIBuffer[i - 1] + 2 * ExtRVIBuffer[i - 2] + ExtRVIBuffer[i - 3]) /
        6
  end
  return ExtSignalBuffer
end

function AO()
  local sma5 = SMA(MEDIAN, 5)
  local sma34 = SMA(MEDIAN, 34)
  local colorbuffer = { [1] = CLR_GREEN }
  local out = {}
  local offset = #sma5 - #sma34

  for i = 1, #sma34 do
    out[i] = sma5[i + offset] - sma34[i]
    if i ~= 1 then
      colorbuffer[i] = (out[i] > out[i - 1]) and CLR_GREEN or CLR_RED
    end
  end
  return out, colorbuffer
end

function AC()
  local ao, _ = AO()
  local ao5 = SMA(ao, 5)
  local colorbuffer = { [1] = CLR_GREEN }
  local out = {}
  local offset = #ao - #ao5

  for i = 1, #ao5 do
    out[i] = ao[i + offset] - ao5[i]
    if i ~= 1 then
      colorbuffer[i] = (out[i] > out[i - 1]) and CLR_GREEN or CLR_RED
    end
  end
  return out, colorbuffer
end

function AD()
  local out = {}
  local cum_sum = 0
  for i = 1, #CLOSE do
    local hi, lo, cl = HIGH[i], LOW[i], CLOSE[i]
    local hl_diff = hi - lo
    if hl_diff ~= 0 then
      cum_sum = cum_sum + (((cl - lo) - (hi - cl)) / hl_diff) * VOLUME[i]
    end
    out[i] = cum_sum
  end
  return out
end

function W_AD()
  local ExtWADBuffer = { [1] = 0.0 }
  for i = 2, #CLOSE do
    local hi, lo, cl, prev_cl = HIGH[i], LOW[i], CLOSE[i], CLOSE[i - 1]
    local trh = math_max(hi, prev_cl)
    local trl = math_min(lo, prev_cl)

    if math_abs(cl - prev_cl) < 0.00001 then
      ExtWADBuffer[i] = ExtWADBuffer[i - 1]
    elseif cl > prev_cl then
      ExtWADBuffer[i] = ExtWADBuffer[i - 1] + cl - trl
    else
      ExtWADBuffer[i] = ExtWADBuffer[i - 1] + cl - trh
    end
  end
  return ExtWADBuffer
end

function MACD(source, fast, slow)
  source = resolve_source(source)
  local fastema = EMA(source, fast)
  local slowema = EMA(source, slow)
  local out = {}
  local diff = #fastema - #slowema
  for i = 1, #slowema do
    out[i] = fastema[i + diff] - slowema[i]
  end
  return out
end

function ENVELOPES(source, period, deviation, matype)
  source = resolve_source(source)

  -- Pick moving average type based on matype parameter
  local ma = (matype == 1 and EMA(source, period)) or
      (matype == 2 and SMMA(source, period)) or
      (matype == 3 and LWMA(source, period)) or
      SMA(source, period)

  local upper = {}
  local lower = {}

  local dev_factor = deviation * 0.01
  local upper_factor = 1 + dev_factor
  local lower_factor = 1 - dev_factor

  -- Single loop to compute both bands
  for i = 1, #ma do
    local val = ma[i]
    upper[i] = upper_factor * val
    lower[i] = lower_factor * val
  end

  return upper, lower
end

function WPR(period)
  local hh = HIGHEST(HIGH, period)
  local ll = LOWEST(LOW, period)
  local out = {}
  for i = 1, #hh - period + 1 do
    local idx = i + period - 1
    local range = hh[idx] - ll[idx]
    out[i] = (range ~= 0) and ((hh[idx] - CLOSE[idx]) / range * -100) or 0
  end
  return out
end

function OSMA(source, fast_ema_period, slow_ema_period, signal_period)
  local macd = MACD(source, fast_ema_period, slow_ema_period)
  local signal = SMA(macd, signal_period)
  local out = {}
  local offset = #macd - #signal
  for i = 1, #signal do
    out[i] = macd[i + offset] - signal[i]
  end
  return out
end

-- Lua 5.1 - Full Stochastic Oscillator (%K Main & %D Signal)
-- Karmaşıklık: O(N) Monotonic Deque + O(1) Sliding Window Sum
-- Girdi: HIGH, LOW, CLOSE dizileri global/kapsam dışı scopetan okunur.

function STOCHASTIC(Kperiod, slowing, Dperiod)
    Kperiod = Kperiod or 5
    slowing = slowing or 3
    Dperiod = Dperiod or 3

    local n = #CLOSE
    local ExtMainBuffer = {}   -- %K (Main Line)
    local ExtSignalBuffer = {} -- %D (Signal Line)

    -- 1. Tüm dizileri 0.0 ile doldur (Nil-free garanti)
    for i = 1, n do
        ExtMainBuffer[i] = 0.0
        ExtSignalBuffer[i] = 0.0
    end

    if n < (Kperiod + slowing - 1) then
        return ExtMainBuffer, ExtSignalBuffer
    end

    -- 2. O(N) Min/Max takibi için Monotonic Deque yapıları
    local max_deque_head, max_deque_tail = 1, 0
    local max_deque_idx = {}

    local min_deque_head, min_deque_tail = 1, 0
    local min_deque_idx = {}

    local ExtHighesBuffer = {}
    local ExtLowesBuffer = {}

    for i = 1, n do
        -- Highest High Deque Güncelleme
        while max_deque_tail >= max_deque_head and HIGH[max_deque_idx[max_deque_tail]] <= HIGH[i] do
            max_deque_tail = max_deque_tail - 1
        end
        max_deque_tail = max_deque_tail + 1
        max_deque_idx[max_deque_tail] = i

        if max_deque_idx[max_deque_head] <= i - Kperiod then
            max_deque_head = max_deque_head + 1
        end
        ExtHighesBuffer[i] = HIGH[max_deque_idx[max_deque_head]]

        -- Lowest Low Deque Güncelleme
        while min_deque_tail >= min_deque_head and LOW[min_deque_idx[min_deque_tail]] >= LOW[i] do
            min_deque_tail = min_deque_tail - 1
        end
        min_deque_tail = min_deque_tail + 1
        min_deque_idx[min_deque_tail] = i

        if min_deque_idx[min_deque_head] <= i - Kperiod then
            min_deque_head = min_deque_head + 1
        end
        ExtLowesBuffer[i] = LOW[min_deque_idx[min_deque_head]]
    end

    -- 3. %K (Main) için Kayan Toplam Değişkenleri
    local sumlow = 0.0
    local sumhigh = 0.0
    local first_valid = Kperiod + slowing - 1

    -- İlk slowing penceresinin ön toplamı (Ön Isınma)
    for k = Kperiod, first_valid do
        sumlow = sumlow + (CLOSE[k] - ExtLowesBuffer[k])
        sumhigh = sumhigh + (ExtHighesBuffer[k] - ExtLowesBuffer[k])
    end

    if sumhigh == 0.0 then
        ExtMainBuffer[first_valid] = 50.0
    else
        ExtMainBuffer[first_valid] = (sumlow / sumhigh) * 100.0
    end

    -- 4. %K Hesabı Döngüsü
    for i = first_valid + 1, n do
        local old_idx = i - slowing

        sumlow  = sumlow - (CLOSE[old_idx] - ExtLowesBuffer[old_idx])
        sumhigh = sumhigh - (ExtHighesBuffer[old_idx] - ExtLowesBuffer[old_idx])

        sumlow  = sumlow + (CLOSE[i] - ExtLowesBuffer[i])
        sumhigh = sumhigh + (ExtHighesBuffer[i] - ExtLowesBuffer[i])

        if sumhigh == 0.0 then
            ExtMainBuffer[i] = 50.0
        else
            ExtMainBuffer[i] = (sumlow / sumhigh) * 100.0
        end
    end

    -- 5. %D (Signal Line) Hesabı - O(1) Sliding Window Sum
    local sum_d = 0.0
    local first_d_valid = first_valid + Dperiod - 1

    if n >= first_d_valid then
        -- İlk %D penceresi için ön toplam
        for k = first_valid, first_d_valid do
            sum_d = sum_d + ExtMainBuffer[k]
        end
        ExtSignalBuffer[first_d_valid] = sum_d / Dperiod

        -- Kayan pencere ile %D güncellemeleri
        for i = first_d_valid + 1, n do
            sum_d = sum_d - ExtMainBuffer[i - Dperiod] + ExtMainBuffer[i]
            ExtSignalBuffer[i] = sum_d / Dperiod
        end
    end

    -- Çift Tampon / Dizi Dönüşü
    return ExtMainBuffer, ExtSignalBuffer
end

function TRIX(source, period)
  local ema3 = EMA(EMA(EMA(source, period), period), period)
  local out = {}
  for i = 2, #ema3 do
    out[i - 1] = (ema3[i] - ema3[i - 1]) / ema3[i - 1]
  end
  return out
end

function DEMA(source, period)
  source = resolve_source(source)
  local ema1 = EMA(source, period)
  local ema2 = EMA(ema1, period)
  local out = {}
  local offset = #ema1 - #ema2
  for i = 1, #ema2 do
    out[i] = 2 * ema1[i + offset] - ema2[i]
  end
  return out
end

function TEMA(source, period)
  source = resolve_source(source)
  local ema1 = EMA(source, period)
  local ema2 = EMA(ema1, period)
  local ema3 = EMA(ema2, period)
  local out = {}
  local offset1 = #ema1 - #ema3
  local offset2 = #ema2 - #ema3

  for i = 1, #ema3 do
    out[i] = 3 * ema1[i + offset1] - 3 * ema2[i + offset2] + ema3[i]
  end
  return out
end

function DPO(source, period)
  source = resolve_source(source)
  local ExtMAPeriod = math_floor(period * 0.5) + 1
  local sma = SMA(source, ExtMAPeriod)
  local out = {}
  for j = 1, #sma do
    out[j] = source[j + ExtMAPeriod - 1] - sma[j]
  end
  return out
end

function CHAIKIN(fast, slow, matype)
  local ad = AD()
  local fastad = (matype == 1 and EMA(ad, fast)) or (matype == 2 and SMMA(ad, fast)) or (matype == 3 and LWMA(ad, fast)) or
      SMA(ad, fast)
  local slowad = (matype == 1 and EMA(ad, slow)) or (matype == 2 and SMMA(ad, slow)) or (matype == 3 and LWMA(ad, slow)) or
      SMA(ad, slow)

  local out = {}
  local diff = #fastad - #slowad
  for i = 1, #slowad do
    out[i] = fastad[i + diff] - slowad[i]
  end
  return out
end

function ROC(source, period)
  source = resolve_source(source)
  local out = {}
  for i = period + 1, #source do
    out[i - period] = ((source[i] - source[i - period]) / source[i]) * 100
  end
  return out
end

function PVT()
  local out = { [1] = 0 }
  for i = 2, #CLOSE do
    out[i] = ((CLOSE[i] - CLOSE[i - 1]) / CLOSE[i - 1]) * VOLUME[i] + out[i - 1]
  end
  return out
end

function ASI(InpT)
  local ExtASIBuffer, ExtSIBuffer, ExtTRBuffer = { 0.0 }, { 0.0 }, { HIGH[1] - LOW[1] }
  local inv_InpT = (InpT ~= 0) and (1 / InpT) or 0

  for i = 2, #CLOSE do
    local dPrevClose, dPrevOpen = CLOSE[i - 1], OPEN[i - 1]
    local dClose, dHigh, dLow   = CLOSE[i], HIGH[i], LOW[i]

    ExtTRBuffer[i]              = math_max(dHigh, dPrevClose) - math_min(dLow, dPrevClose)
    local ER                    = 0.0
    if dPrevClose > dHigh then
      ER = dPrevClose - dHigh
    elseif dPrevClose < dLow then
      ER = dLow - dPrevClose
    end

    local K = math_max(math_abs(dHigh - dPrevClose), math_abs(dLow - dPrevClose))
    local SH = math_abs(dPrevClose - dPrevOpen)
    local R = ExtTRBuffer[i] - 0.5 * ER + 0.25 * SH

    if R == 0.0 or inv_InpT == 0 then
      ExtSIBuffer[i] = 0.0
    else
      ExtSIBuffer[i] = 50 * (dClose - dPrevClose + 0.5 * (dClose - OPEN[i]) + 0.25 * (dPrevClose - dPrevOpen)) *
          (K * inv_InpT) / R
    end
    ExtASIBuffer[i] = ExtASIBuffer[i - 1] + ExtSIBuffer[i]
  end
  return ExtASIBuffer
end

function MI(ExtPeriodEMA, ExtSecondPeriodEMA, ExtSumPeriod)
  local function FastExpMA(price, period, prev_val, pos)
    if period <= 0 then return 0.0 end
    local pr = 2.0 / (period + 1.0)
    return price[pos] * pr + prev_val * (1 - pr)
  end

  local posMI = ExtSumPeriod + ExtPeriodEMA + ExtSecondPeriodEMA - 3
  local n = #CLOSE
  local ExtHLBuffer, ExtEHLBuffer, ExtEEHLBuffer = {}, {}, {}
  local ExtMIBuffer = {}

  ExtEHLBuffer[1], ExtEEHLBuffer[1] = 0, 0

  for i = 1, n do
    ExtHLBuffer[i] = HIGH[i] - LOW[i]
    if i == 1 then
      ExtEHLBuffer[1]  = FastExpMA(ExtHLBuffer, ExtPeriodEMA, 0, 1)
      ExtEEHLBuffer[1] = FastExpMA(ExtEHLBuffer, ExtSecondPeriodEMA, 0, 1)
    else
      ExtEHLBuffer[i]  = FastExpMA(ExtHLBuffer, ExtPeriodEMA, ExtEHLBuffer[i - 1], i)
      ExtEEHLBuffer[i] = FastExpMA(ExtEHLBuffer, ExtSecondPeriodEMA, ExtEEHLBuffer[i - 1], i)
    end

    if i - 1 > posMI then
      local dTmp = 0.0
      for j = 1, ExtSumPeriod do
        dTmp = dTmp + (ExtEHLBuffer[i - j] / ExtEEHLBuffer[i - j])
      end
      ExtMIBuffer[i - posMI - 1] = dTmp
    end
  end
  return ExtMIBuffer
end

function PLUSDI(period)
  local ExtPDBuffer = { [1] = 0 }
  for i = 2, #CLOSE do
    local Hi, prevHi = HIGH[i], HIGH[i - 1]
    local Lo, prevLo = LOW[i], LOW[i - 1]
    local prevCl = CLOSE[i - 1]

    local dTmpP = math_max(0.0, Hi - prevHi)
    local dTmpN = math_max(0.0, prevLo - Lo)

    if dTmpP <= dTmpN then dTmpP = 0.0 end

    local tr = math_max(math_abs(Hi - Lo), math_abs(Hi - prevCl), math_abs(Lo - prevCl))
    ExtPDBuffer[i] = (tr ~= 0.0) and (100.0 * dTmpP / tr) or 0.0
  end
  return EMA(ExtPDBuffer, period)
end

function MINUSDI(period)
  local ExtNDBuffer = { [1] = 0 }
  for i = 2, #CLOSE do
    local Hi, prevHi = HIGH[i], HIGH[i - 1]
    local Lo, prevLo = LOW[i], LOW[i - 1]
    local prevCl = CLOSE[i - 1]

    local dTmpP = math_max(0.0, Hi - prevHi)
    local dTmpN = math_max(0.0, prevLo - Lo)

    if dTmpP >= dTmpN then dTmpN = 0.0 end

    local tr = math_max(math_abs(Hi - Lo), math_abs(Hi - prevCl), math_abs(Lo - prevCl))
    ExtNDBuffer[i] = (tr ~= 0.0) and (100.0 * dTmpN / tr) or 0.0
  end
  return EMA(ExtNDBuffer, period)
end

function ADX(ExtADXPeriod)
  local ExtPDIBuffer = PLUSDI(ExtADXPeriod)
  local ExtNDIBuffer = MINUSDI(ExtADXPeriod)
  local ExtTmpBuffer = {}

  for i = 1, #CLOSE do
    local dTmp = ExtPDIBuffer[i] + ExtNDIBuffer[i]
    ExtTmpBuffer[i] = (dTmp ~= 0.0) and (100.0 * math_abs((ExtPDIBuffer[i] - ExtNDIBuffer[i]) / dTmp)) or 0.0
  end
  return EMA(ExtTmpBuffer, ExtADXPeriod)
end

function CHV(ExtCHVPeriod, ExtSmoothPeriod, matype)
  local ExtHLBuffer = {}
  for i = 1, #CLOSE do ExtHLBuffer[i] = HIGH[i] - LOW[i] end

  local ExtSHLBuffer = (matype == 1) and EMA(ExtHLBuffer, ExtSmoothPeriod) or SMA(ExtHLBuffer, ExtSmoothPeriod)
  local ExtCHVBuffer = {}

  for i = 1 + ExtCHVPeriod, #ExtSHLBuffer do
    local prev = ExtSHLBuffer[i - ExtCHVPeriod]
    ExtCHVBuffer[i - ExtCHVPeriod] = (prev ~= 0) and (100.0 * (ExtSHLBuffer[i] - prev) / prev) or 0
  end
  return ExtCHVBuffer
end

function AMA(price, amaperiod, ExtFastPeriodEMA, ExtSlowPeriodEMA)
  price = resolve_source(price)
  local ExtFastSC = 2.0 / (ExtFastPeriodEMA + 1.0)
  local ExtSlowSC = 2.0 / (ExtSlowPeriodEMA + 1.0)
  local ExtAMABuffer = {}

  for i = 1 + amaperiod, #CLOSE do
    local dSignal = math_abs(price[i] - price[i - amaperiod])
    local dNoise = 0.0
    for delta = 0, amaperiod - 1 do
      dNoise = dNoise + math_abs(price[i - delta] - price[i - delta - 1])
    end

    local ER = (dNoise ~= 0.0) and (dSignal / dNoise) or 0.0
    local dCurrentSSC = (ER * (ExtFastSC - ExtSlowSC)) + ExtSlowSC
    local dPrevAMA = (i ~= 1 + amaperiod) and ExtAMABuffer[i - amaperiod - 1] or price[i - 1]

    ExtAMABuffer[i - amaperiod] = (dCurrentSSC * dCurrentSSC) * (price[i] - dPrevAMA) + dPrevAMA
  end
  return ExtAMABuffer
end

function FAMA(price, InpPeriodFrAMA)
  price = resolve_source(price)
  local limit = 2 * InpPeriodFrAMA
  local FrAmaBuffer = { [1] = price[limit - 1] }
  local inv_ln2 = 1 / math_log(2.0)

  for i = limit, #price do
    local Hi1, Lo1 = HIGH[i], LOW[i]
    local Hi2, Lo2 = HIGH[i - InpPeriodFrAMA], LOW[i - InpPeriodFrAMA]
    local Hi3, Lo3 = HIGH[i], LOW[i]

    for j = i - InpPeriodFrAMA + 1, i do
      if HIGH[j] > Hi1 then Hi1 = HIGH[j] end
      if LOW[j] < Lo1 then Lo1 = LOW[j] end
    end
    for j = i - 2 * InpPeriodFrAMA + 1, i - InpPeriodFrAMA do
      if HIGH[j] > Hi2 then Hi2 = HIGH[j] end
      if LOW[j] < Lo2 then Lo2 = LOW[j] end
    end
    for j = i - 2 * InpPeriodFrAMA + 1, i do
      if HIGH[j] > Hi3 then Hi3 = HIGH[j] end
      if LOW[j] < Lo3 then Lo3 = LOW[j] end
    end

    local N1 = (Hi1 - Lo1) / InpPeriodFrAMA
    local N2 = (Hi2 - Lo2) / InpPeriodFrAMA
    local N3 = (Hi3 - Lo3) / (2 * InpPeriodFrAMA)

    local D = ((N1 + N2 > 0) and (N3 > 0)) and ((math_log(N1 + N2) - math_log(N3)) * inv_ln2) or 0
    local ALFA = math_exp(-4.6 * (D - 1.0))
    local idx = i - limit + 2
    FrAmaBuffer[idx] = ALFA * price[i] + (1 - ALFA) * FrAmaBuffer[idx - 1]
  end
  return FrAmaBuffer
end

function PCUP(period)
  local out = {}
  for i = period + 1, #HIGH do
    local res = HIGH[i]
    for j = i - period + 1, i - 1 do
      if HIGH[j] > res then res = HIGH[j] end
    end
    out[i - period] = res
  end
  return out
end

function PCDOWN(period)
  local out = {}
  for i = period + 1, #LOW do
    local res = LOW[i]
    for j = i - period + 1, i - 1 do
      if LOW[j] < res then res = LOW[j] end
    end
    out[i - period] = res
  end
  return out
end

function PCMID(period)
  local up, down = PCUP(period), PCDOWN(period)
  local out = {}
  for i = 1, #up do out[i] = (up[i] + down[i]) * 0.5 end
  return out
end

--- Calculate Senkou Span A and Senkou Span B
function Ichimoku(tenkan_period, kijun_period, senkou_period)
  -- Helper: Find maximum value over a range going backwards from `from_index`
  local function highest(array, range, from_index)
    local res = array[from_index]
    local limit = math.max(1, from_index - range + 1)
    for i = from_index, limit, -1 do
      if array[i] > res then
        res = array[i]
      end
    end
    return res
  end

  -- Helper: Find minimum value over a range going backwards from `from_index`
  local function lowest(array, range, from_index)
    local res = array[from_index]
    local limit = math.max(1, from_index - range + 1)
    for i = from_index, limit, -1 do
      if array[i] < res then
        res = array[i]
      end
    end
    return res
  end

  local rates_total = #HIGH

  local span_a_buffer = {}
  local span_b_buffer = {}
  local cloud = {}
  local color = {}

  for i = 1, rates_total do
    -- 1. Calculate Tenkan-sen (Highest High + Lowest Low) / 2 over Tenkan period
    local tenkan_max = highest(HIGH, tenkan_period, i)
    local tenkan_min = lowest(LOW, tenkan_period, i)
    local tenkan_val = (tenkan_max + tenkan_min) / 2.0

    -- 2. Calculate Kijun-sen (Highest High + Lowest Low) / 2 over Kijun period
    local kijun_max = highest(HIGH, kijun_period, i)
    local kijun_min = lowest(LOW, kijun_period, i)
    local kijun_val = (kijun_max + kijun_min) / 2.0

    -- 3. Senkou Span A: (Tenkan-sen + Kijun-sen) / 2
    span_a_buffer[i] = (tenkan_val + kijun_val) / 2.0

    -- 4. Senkou Span B: (Highest High + Lowest Low) / 2 over Senkou period
    local senkou_max = highest(HIGH, senkou_period, i)
    local senkou_min = lowest(LOW, senkou_period, i)
    span_b_buffer[i] = (senkou_max + senkou_min) / 2.0

    if span_b_buffer[i] < span_a_buffer[i] then
      table.insert(color, CLR_GREEN)
      table.insert(color, CLR_GREEN)
    else
      table.insert(color, CLR_RED)
      table.insert(color, CLR_RED)
    end

    table.insert(cloud, span_a_buffer[i])
    table.insert(cloud, span_b_buffer[i])
  end


  return span_a_buffer, span_b_buffer, cloud, color
end

--------------------------------------------------------------------------------
-- ICHIMOKU KINKO HYO
--------------------------------------------------------------------------------
function TENKAN(InpTenkan)
  local ExtTenkanBuffer = {}
  for i = InpTenkan + 1, #CLOSE do
    local _high, _low = HIGH[i], LOW[i]
    for j = i - InpTenkan + 1, i - 1 do
      if HIGH[j] > _high then _high = HIGH[j] end
      if LOW[j] < _low then _low = LOW[j] end
    end
    ExtTenkanBuffer[i - InpTenkan] = (_high + _low) * 0.5
  end
  return ExtTenkanBuffer
end

function KIJUN(InpKijun)
  local ExtKijunBuffer = {}
  for i = InpKijun + 1, #CLOSE do
    local _high, _low = HIGH[i], LOW[i]
    for j = i - InpKijun + 1, i - 1 do
      if HIGH[j] > _high then _high = HIGH[j] end
      if LOW[j] < _low then _low = LOW[j] end
    end
    ExtKijunBuffer[i - InpKijun] = (_high + _low) * 0.5
  end
  return ExtKijunBuffer
end

function SPANA(InpTenkan, InpKijun)
  local ExtSpanABuffer  = {}
  local ExtTenkanBuffer = TENKAN(InpTenkan)
  local ExtKijunBuffer  = KIJUN(InpKijun)
  local fark            = InpKijun - InpTenkan

  for i = 1, #ExtKijunBuffer do
    ExtSpanABuffer[i] = (ExtTenkanBuffer[i + fark] + ExtKijunBuffer[i]) * 0.5
  end
  return ExtSpanABuffer
end

function SPANB(InpSenkou)
  local ExtSpanBBuffer = {}
  for i = InpSenkou + 1, #CLOSE do
    local _high, _low = HIGH[i], LOW[i]
    for j = i - InpSenkou + 1, i - 1 do
      if HIGH[j] > _high then _high = HIGH[j] end
      if LOW[j] < _low then _low = LOW[j] end
    end
    ExtSpanBBuffer[i - InpSenkou] = (_high + _low) * 0.5
  end
  return ExtSpanBBuffer
end

function AROON(period)
  local up, down = {}, {}
  local inv_period = 100 / period

  for i = period, #CLOSE do
    local h_res, h_ago = HIGH[i - period + 1], period
    local l_res, l_ago = LOW[i - period + 1], period

    for k = 1, period do
      local idx = i - period + k
      if HIGH[idx] >= h_res then h_res, h_ago = HIGH[idx], period - k end
      if LOW[idx] <= l_res then l_res, l_ago = LOW[idx], period - k end
    end
    local idx_out = i - period + 1
    up[idx_out]   = (period - h_ago) * inv_period
    down[idx_out] = (period - l_ago) * inv_period
  end
  return up, down
end

--------------------------------------------------------------------------------
-- SPECIALTY MOVING AVERAGES & CHANNELS
--------------------------------------------------------------------------------
function VIDYA(source, InpPeriodCMO, InpPeriodEMA)
  source = resolve_source(source)
  local VIDYA_Buffer = {}
  local n = #CLOSE

  for i = 1, InpPeriodCMO + InpPeriodEMA - 1 do VIDYA_Buffer[i] = CLOSE[i] end

  local ExtF = 2.0 / (1.0 + InpPeriodEMA)
  for i = InpPeriodCMO + InpPeriodEMA, n do
    local UpSum, DownSum = 0.0, 0.0
    for k = 0, InpPeriodCMO - 1 do
      local diff = source[i - k] - source[i - k - 1]
      if diff > 0.0 then UpSum = UpSum + diff else DownSum = DownSum - diff end
    end

    local mulCMO = (UpSum + DownSum ~= 0.0) and math_abs((UpSum - DownSum) / (UpSum + DownSum)) or 0.0
    VIDYA_Buffer[i] = source[i] * ExtF * mulCMO + VIDYA_Buffer[i - 1] * (1 - ExtF * mulCMO)
  end
  return VIDYA_Buffer
end

function VROC(ExtPeriodVROC)
  local ExtVROCBuffer = {}
  for i = ExtPeriodVROC, #CLOSE do
    local PrevVolume = VOLUME[i - (ExtPeriodVROC - 1)]
    local CurrVolume = VOLUME[i]
    local idx = i - ExtPeriodVROC + 1

    if PrevVolume ~= 0 then
      ExtVROCBuffer[idx] = 100.0 * (CurrVolume - PrevVolume) / PrevVolume
    else
      ExtVROCBuffer[idx] = ExtVROCBuffer[idx - 1] or 0
    end
  end
  return ExtVROCBuffer
end

function PSAR(ExtSarStep, ExtSarMaximum)
  local ExtAFBuffer, ExtSARBuffer, ExtEPBuffer = {}, {}, {}
  local pos                                    = 2

  ExtAFBuffer[1], ExtAFBuffer[2]               = ExtSarStep, ExtSarStep
  ExtSARBuffer[1]                              = HIGH[1]
  ExtEPBuffer[1], ExtEPBuffer[2]               = LOW[pos], LOW[pos]

  local ExtLastRevPos                          = 1
  local ExtDirectionLong                       = false
  ExtSARBuffer[2]                              = math_max(HIGH[1], HIGH[2])

  for i = pos, #CLOSE - 1 do
    if ExtDirectionLong then
      if ExtSARBuffer[i] > LOW[i] then
        ExtDirectionLong = false
        local max_h = HIGH[ExtLastRevPos]
        for j = ExtLastRevPos + 1, i do if HIGH[j] > max_h then max_h = HIGH[j] end end
        ExtSARBuffer[i] = max_h
        ExtEPBuffer[i]  = LOW[i]
        ExtLastRevPos   = i
        ExtAFBuffer[i]  = ExtSarStep
      end
    else
      if ExtSARBuffer[i] < HIGH[i] then
        ExtDirectionLong = true
        local min_l = LOW[ExtLastRevPos]
        for j = ExtLastRevPos + 1, i do if LOW[j] < min_l then min_l = LOW[j] end end
        ExtSARBuffer[i] = min_l
        ExtEPBuffer[i]  = HIGH[i]
        ExtLastRevPos   = i
        ExtAFBuffer[i]  = ExtSarStep
      end
    end

    if ExtDirectionLong then
      if HIGH[i] > ExtEPBuffer[i - 1] and i ~= ExtLastRevPos then
        ExtEPBuffer[i] = HIGH[i]
        ExtAFBuffer[i] = math_min(ExtSarMaximum, ExtAFBuffer[i - 1] + ExtSarStep)
      elseif i ~= ExtLastRevPos then
        ExtAFBuffer[i] = ExtAFBuffer[i - 1]
        ExtEPBuffer[i] = ExtEPBuffer[i - 1]
      end
      ExtSARBuffer[i + 1] = ExtSARBuffer[i] + ExtAFBuffer[i] * (ExtEPBuffer[i] - ExtSARBuffer[i])
      if ExtSARBuffer[i + 1] > LOW[i] or ExtSARBuffer[i + 1] > LOW[i - 1] then
        ExtSARBuffer[i + 1] = math_min(LOW[i], LOW[i - 1])
      end
    else
      if LOW[i] < ExtEPBuffer[i - 1] and i ~= ExtLastRevPos then
        ExtEPBuffer[i] = LOW[i]
        ExtAFBuffer[i] = math_min(ExtSarMaximum, ExtAFBuffer[i - 1] + ExtSarStep)
      elseif i ~= ExtLastRevPos then
        ExtAFBuffer[i] = ExtAFBuffer[i - 1]
        ExtEPBuffer[i] = ExtEPBuffer[i - 1]
      end
      ExtSARBuffer[i + 1] = ExtSARBuffer[i] + ExtAFBuffer[i] * (ExtEPBuffer[i] - ExtSARBuffer[i])
      if ExtSARBuffer[i + 1] < HIGH[i] or ExtSARBuffer[i + 1] < HIGH[i - 1] then
        ExtSARBuffer[i + 1] = math_max(HIGH[i], HIGH[i - 1])
      end
    end
  end
  return ExtSARBuffer
end

function CEXITLONG(period, ATRPeriod, MultipleATR)
  local hhv, atr = HIGHEST(HIGH, period), ATR(ATRPeriod)
  local diff, out = #hhv - #atr, {}
  for i = 1, #atr do out[i] = hhv[i + diff] - atr[i] * MultipleATR end
  return out
end

function CEXITSHORT(period, ATRPeriod, MultipleATR)
  local llv, atr = LOWEST(LOW, period), ATR(ATRPeriod)
  local diff, out = #llv - #atr, {}
  for i = 1, #atr do out[i] = llv[i + diff] + atr[i] * MultipleATR end
  return out
end

function KELTNERUP(price, period, multiplier)
  local ema, atr = EMA(price, period), ATR(period)
  local diff, out = #ema - #atr, {}
  for i = 1, #atr do out[i] = ema[i + diff] + atr[i] * multiplier end
  return out
end

function KELTNERDOWN(price, period, multiplier)
  local ema, atr = EMA(price, period), ATR(period)
  local diff, out = #ema - #atr, {}
  for i = 1, #atr do out[i] = ema[i + diff] - atr[i] * multiplier end
  return out
end

function COPP(r1period, r2period, wperiod)
  local roc1, roc2 = ROC(CLOSE, r1period), ROC(CLOSE, r2period)
  local diff, out = #roc2 - #roc1, {}
  for i = 1, #roc1 do out[i] = roc1[i] + roc2[i + diff] end
  return LWMA(out, wperiod)
end

function STOCHRSI(period)
  local rsi = RSI(PRICE_CLOSE, period)
  local llv, hhv = LOWEST(rsi, period), HIGHEST(rsi, period)
  local out = {}
  for i = period, #hhv do
    local range = hhv[i] - llv[i]
    out[i - period + 1] = (range ~= 0) and ((rsi[i] - llv[i]) / range) or 0
  end
  return SMA(out, 3)
end

function SHIFT(source, period)
  local out = {}
  for i = period, #source do out[i - period] = source[i - period] end
  return out
end

function UltimateOsc(InpFastPeriod, InpMiddlePeriod, InpSlowPeriod, InpFastK, InpMiddleK, InpSlowK)
  local BP, TR = {}, {}
  local n = #CLOSE

  for i = 2, n do
    local prev_close = CLOSE[i - 1]
    BP[i] = CLOSE[i] - math_min(LOW[i], prev_close)
    TR[i] = math_max(HIGH[i], prev_close) - math_min(LOW[i], prev_close)
  end

  local sumBP_fast, sumTR_fast = 0, 0
  local sumBP_mid, sumTR_mid   = 0, 0
  local sumBP_slow, sumTR_slow = 0, 0

  for i = 2, InpSlowPeriod + 1 do
    local bp, tr = BP[i], TR[i]
    if i > InpSlowPeriod + 1 - InpFastPeriod then
      sumBP_fast = sumBP_fast + bp; sumTR_fast = sumTR_fast + tr
    end
    if i > InpSlowPeriod + 1 - InpMiddlePeriod then
      sumBP_mid = sumBP_mid + bp; sumTR_mid = sumTR_mid + tr
    end
    sumBP_slow = sumBP_slow + bp; sumTR_slow = sumTR_slow + tr
  end

  local UO = {}
  local weight_denom = InpFastK + InpMiddleK + InpSlowK

  for i = InpSlowPeriod + 1, n do
    if i > InpSlowPeriod + 1 then
      sumBP_fast = sumBP_fast - BP[i - InpFastPeriod] + BP[i]
      sumTR_fast = sumTR_fast - TR[i - InpFastPeriod] + TR[i]
      sumBP_mid  = sumBP_mid - BP[i - InpMiddlePeriod] + BP[i]
      sumTR_mid  = sumTR_mid - TR[i - InpMiddlePeriod] + TR[i]
      sumBP_slow = sumBP_slow - BP[i - InpSlowPeriod] + BP[i]
      sumTR_slow = sumTR_slow - TR[i - InpSlowPeriod] + TR[i]
    end

    local Avg7            = (sumTR_fast ~= 0) and (sumBP_fast / sumTR_fast) or 0
    local Avg14           = (sumTR_mid ~= 0) and (sumBP_mid / sumTR_mid) or 0
    local Avg28           = (sumTR_slow ~= 0) and (sumBP_slow / sumTR_slow) or 0

    UO[i - InpSlowPeriod] = 100 * ((InpFastK * Avg7) + (InpMiddleK * Avg14) + Avg28) / weight_denom
  end
  return UO
end

function AndeanOsc(InpLength, InpSignalLength)
  local n = #CLOSE
  local Up1, Up2, Dn1, Dn2 = {}, {}, {}, {}
  local Bull, Bear, Signal = {}, {}, {}

  if n == 0 then return Bull, Bear, Signal end

  local alpha     = 2 / (InpLength + 1)
  local alpha_inv = 1 - alpha

  -- 1. Initialize index 1
  local c1, o1    = CLOSE[1], OPEN[1]
  Up1[1]          = math_max(c1, o1)
  Up2[1]          = math_max(c1 * c1, o1 * o1)
  Dn1[1]          = math_min(c1, o1)
  Dn2[1]          = math_min(c1 * c1, o1 * o1)

  Bull[1]         = math_sqrt(math_max(0, Dn2[1] - Dn1[1] * Dn1[1]))
  Bear[1]         = math_sqrt(math_max(0, Up2[1] - Up1[1] * Up1[1]))
  Signal[1]       = math_max(Bull[1], Bear[1])

  -- 2. Process remaining bars
  for i = 2, n do
    local c, o       = CLOSE[i], OPEN[i]
    local c_sq, o_sq = c * c, o * o

    -- Up components (Bullish envelope)
    Up1[i]           = math_max(c, o, Up1[i - 1] * alpha_inv + c * alpha)
    Up2[i]           = math_max(c_sq, o_sq, Up2[i - 1] * alpha_inv + c_sq * alpha)

    -- Down components (Bearish envelope)
    Dn1[i]           = math_min(c, o, Dn1[i - 1] * alpha_inv + c * alpha)
    Dn2[i]           = math_min(c_sq, o_sq, Dn2[i - 1] * alpha_inv + c_sq * alpha)

    -- Variance-based components for Bull & Bear intensity
    Bull[i]          = math_sqrt(math_max(0, Dn2[i] - Dn1[i] * Dn1[i]))
    Bear[i]          = math_sqrt(math_max(0, Up2[i] - Up1[i] * Up1[i]))
    Signal[i]        = math_max(Bull[i], Bear[i])
  end

  -- Smooth the Signal array using EMA
  Signal = EMA(Signal, InpSignalLength)

  return Bull, Bear, Signal
end

function ZIGZAG(ExtDepth, ExtDeviation, ExtBackstep)
  local rates_total = #CLOSE
  local ZigzagBuffer, HighMapBuffer, LowMapBuffer = {}, {}, {}

  for i = 1, rates_total do
    ZigzagBuffer[i], HighMapBuffer[i], LowMapBuffer[i] = 0, 0, 0
  end

  if rates_total < 100 then return ZigzagBuffer end

  local deviation = ExtDeviation * 0.00001
  local lastlow, lasthigh = 0, 0
  local lasthighpos, lastlowpos = 0, 0
  local whatlookfor = 0
  local Pike, Sill = 1, -1

  for shift = ExtDepth, rates_total do
    local min_idx = shift
    for j = math_max(1, shift - ExtDepth + 1), shift do
      if LOW[j] < LOW[min_idx] then min_idx = j end
    end
    local val = LOW[min_idx]

    if val == lastlow then
      val = 0.0
    else
      lastlow = val
      if (LOW[shift] - val) > deviation then
        val = 0.0
      else
        for back = 1, ExtBackstep do
          local res = LowMapBuffer[shift - back]
          if res ~= 0 and res > val then LowMapBuffer[shift - back] = 0.0 end
        end
      end
    end
    LowMapBuffer[shift] = (LOW[shift] == val) and val or 0.0

    local max_idx = shift
    for j = math_max(1, shift - ExtDepth + 1), shift do
      if HIGH[j] > HIGH[max_idx] then max_idx = j end
    end
    val = HIGH[max_idx]

    if val == lasthigh then
      val = 0.0
    else
      lasthigh = val
      if (val - HIGH[shift]) > deviation then
        val = 0.0
      else
        for back = 1, ExtBackstep do
          local res = HighMapBuffer[shift - back]
          if res ~= 0 and res < val then HighMapBuffer[shift - back] = 0.0 end
        end
      end
    end
    HighMapBuffer[shift] = (HIGH[shift] == val) and val or 0.0
  end

  lastlow, lasthigh = 0, 0

  for shift = ExtDepth, rates_total do
    if whatlookfor == 0 then
      if lastlow == 0 and lasthigh == 0 then
        if HighMapBuffer[shift] ~= 0 then
          lasthigh, lasthighpos, whatlookfor = HIGH[shift], shift, Sill
          ZigzagBuffer[shift] = lasthigh
        elseif LowMapBuffer[shift] ~= 0 then
          lastlow, lastlowpos, whatlookfor = LOW[shift], shift, Pike
          ZigzagBuffer[shift] = lastlow
        end
      end
    elseif whatlookfor == Pike then
      if LowMapBuffer[shift] ~= 0.0 and LowMapBuffer[shift] < lastlow and HighMapBuffer[shift] == 0.0 then
        ZigzagBuffer[lastlowpos] = 0.0
        lastlowpos, lastlow = shift, LowMapBuffer[shift]
        ZigzagBuffer[shift] = lastlow
      end
      if HighMapBuffer[shift] ~= 0.0 and LowMapBuffer[shift] == 0.0 then
        lasthigh, lasthighpos = HighMapBuffer[shift], shift
        ZigzagBuffer[shift], whatlookfor = lasthigh, Sill
      end
    elseif whatlookfor == Sill then
      if HighMapBuffer[shift] ~= 0.0 and HighMapBuffer[shift] > lasthigh and LowMapBuffer[shift] == 0.0 then
        ZigzagBuffer[lasthighpos] = 0.0
        lasthighpos, lasthigh = shift, HighMapBuffer[shift]
        ZigzagBuffer[shift] = lasthigh
      end
      if LowMapBuffer[shift] ~= 0.0 and HighMapBuffer[shift] == 0.0 then
        lastlow, lastlowpos = LowMapBuffer[shift], shift
        ZigzagBuffer[shift], whatlookfor = lastlow, Pike
      end
    end
  end
  return ZigzagBuffer
end

function SUPERTREND(Pd, Factor)
  local n = #CLOSE
  local st, colorbuffer = {}, {}
  if Pd > n then return st, colorbuffer end

  local iatr = ATR(Pd)
  if #iatr == 0 then return st, colorbuffer end

  local offset = n - #iatr

  local Up, Dn, trend = {}, {}, {}

  -- Helper to safely extract ATR for bar i
  local function get_atr(i)
    if i <= offset then
      return iatr[1]
    else
      return iatr[i - offset]
    end
  end

  -- 1. Initialize bar 1
  local hl2_1 = (HIGH[1] + LOW[1]) * 0.5
  local atr_1 = get_atr(1)

  Up[1] = hl2_1 + (Factor * atr_1)
  Dn[1] = hl2_1 - (Factor * atr_1)
  trend[1] = 1
  st[1] = Dn[1]
  colorbuffer[1] = CLR_GREEN

  -- 2. Process remaining bars
  for i = 2, n do
    local hl2 = (HIGH[i] + LOW[i]) * 0.5
    local atr_val = get_atr(i)

    -- Calculate raw basic bands
    local basic_up = hl2 + (Factor * atr_val)
    local basic_dn = hl2 - (Factor * atr_val)

    -- Trail lower band (Dn): non-decreasing
    if basic_dn > Dn[i - 1] or CLOSE[i - 1] < Dn[i - 1] then
      Dn[i] = basic_dn
    else
      Dn[i] = Dn[i - 1]
    end

    -- Trail upper band (Up): non-increasing
    if basic_up < Up[i - 1] or CLOSE[i - 1] > Up[i - 1] then
      Up[i] = basic_up
    else
      Up[i] = Up[i - 1]
    end

    -- Determine trend state
    if trend[i - 1] == -1 and CLOSE[i] > Up[i - 1] then
      trend[i] = 1
    elseif trend[i - 1] == 1 and CLOSE[i] < Dn[i - 1] then
      trend[i] = -1
    else
      trend[i] = trend[i - 1]
    end

    -- Assign output value and color
    if trend[i] == 1 then
      st[i] = Dn[i]
      colorbuffer[i] = CLR_GREEN
    else
      st[i] = Up[i]
      colorbuffer[i] = CLR_RED
    end
  end

  return st, colorbuffer
end

-- Slot Indices (1-based offsets into flat state blocks of size 16)
local P_PRICE     = 1
local P_SMOOTH    = 2
local P_DETRENDER = 3
local P_PERIOD    = 4
local P_PHASE     = 5
local P_Q1        = 6
local P_I1        = 7
local P_JI        = 8
local P_JQ        = 9
local P_Q2        = 10
local P_I2        = 11
local P_RE        = 12
local P_IM        = 13
local P_SA        = 14
local P_MAMA      = 15
local P_FAMA      = 16

-- Constants
local RAD2DEG     = 180.0 / math.pi
local DEG2RAD     = math.pi / 180.0 -- Used if Lua version atan expects single argument in radians

function MamaFamaOsc(src, inpFastLimit, inpSlowLimit)
  local n = #src
  if n == 0 then return {}, {} end

  -- Pre-allocate output tables
  local valm = {}
  local valf = {}

  -- Pre-allocate a flat array for 16 slots per bar
  local STRIDE = 16
  local workMama = {}
  for k = 1, n * STRIDE do workMama[k] = 0.0 end

  -- Loop over all bars
  for r = 0, n - 1 do
    local idx = r + 1
    local base = (idx - 1) * STRIDE
    local price = src[idx]

    workMama[base + P_PRICE] = price

    -- 1. Smooth calculation
    local smooth
    if r > 3 then
      local p0 = price
      local p1 = workMama[(base - STRIDE) + P_PRICE]
      local p2 = workMama[(base - 2 * STRIDE) + P_PRICE]
      local p3 = workMama[(base - 3 * STRIDE) + P_PRICE]
      smooth = (4.0 * p0 + 3.0 * p1 + 2.0 * p2 + p3) * 0.1
    else
      smooth = price
    end
    workMama[base + P_SMOOTH] = smooth

    -- 2. Hilbert Transform Components (Inlined calcComp logic)
    local mult = 0.075 * (workMama[(base - STRIDE) + P_PERIOD] or 0) + 0.54

    -- Detrender
    local detrender
    if idx > 6 then
      local s0 = smooth
      local s2 = workMama[(base - 2 * STRIDE) + P_SMOOTH]
      local s4 = workMama[(base - 4 * STRIDE) + P_SMOOTH]
      local s6 = workMama[(base - 6 * STRIDE) + P_SMOOTH]
      detrender = (0.0962 * s0 + 0.5769 * s2 - 0.5769 * s4 - 0.0962 * s6) * mult
    else
      detrender = smooth
    end
    workMama[base + P_DETRENDER] = detrender

    -- Q1
    local q1
    if idx > 6 then
      local d0 = detrender
      local d2 = workMama[(base - 2 * STRIDE) + P_DETRENDER]
      local d4 = workMama[(base - 4 * STRIDE) + P_DETRENDER]
      local d6 = workMama[(base - 6 * STRIDE) + P_DETRENDER]
      q1 = (0.0962 * d0 + 0.5769 * d2 - 0.5769 * d4 - 0.0962 * d6) * mult
    else
      q1 = detrender
    end
    workMama[base + P_Q1] = q1

    -- I1
    local i1 = (r > 2) and workMama[(base - 3 * STRIDE) + P_DETRENDER] or detrender
    workMama[base + P_I1] = i1

    -- jI
    local ji
    if idx > 6 then
      local i1_0 = i1
      local i1_2 = workMama[(base - 2 * STRIDE) + P_I1]
      local i1_4 = workMama[(base - 4 * STRIDE) + P_I1]
      local i1_6 = workMama[(base - 6 * STRIDE) + P_I1]
      ji = (0.0962 * i1_0 + 0.5769 * i1_2 - 0.5769 * i1_4 - 0.0962 * i1_6) * mult
    else
      ji = i1
    end
    workMama[base + P_JI] = ji

    -- jQ
    local jq
    if idx > 6 then
      local q1_0 = q1
      local q1_2 = workMama[(base - 2 * STRIDE) + P_Q1]
      local q1_4 = workMama[(base - 4 * STRIDE) + P_Q1]
      local q1_6 = workMama[(base - 6 * STRIDE) + P_Q1]
      jq = (0.0962 * q1_0 + 0.5769 * q1_2 - 0.5769 * q1_4 - 0.0962 * q1_6) * mult
    else
      jq = q1
    end
    workMama[base + P_JQ] = jq

    -- 3. Quadrature Phase & Real/Imaginary Components
    local prev_base = base - STRIDE
    local i2, q2, re, im

    if r == 0 then
      i2 = i1
      q2 = q1
      re = i2
      im = i2
    else
      local prev_i2 = workMama[prev_base + P_I2]
      local prev_q2 = workMama[prev_base + P_Q2]
      i2 = 0.2 * (i1 - jq) + 0.8 * prev_i2
      q2 = 0.2 * (q1 + ji) + 0.8 * prev_q2
      re = 0.2 * (i2 * prev_i2 + q2 * prev_q2) + 0.8 * workMama[prev_base + P_RE]
      im = 0.2 * (i2 * prev_q2 - q2 * prev_i2) + 0.8 * workMama[prev_base + P_IM]
    end

    workMama[base + P_I2] = i2
    workMama[base + P_Q2] = q2
    workMama[base + P_RE] = re
    workMama[base + P_IM] = im

    -- 4. Period & Phase Calculation
    local period = 0.0
    if re ~= 0 and im ~= 0 then
      period = 360.0 / (math_atan(im / re) * RAD2DEG)
    end

    if r > 0 then
      local prev_period = workMama[prev_base + P_PERIOD]
      period = math_min(period, 1.50 * prev_period)
      period = math_max(period, 0.67 * prev_period)
    end

    period = math_min(math_max(period ~= 0 and period or 6.0, 6.0), 50.0)

    if r > 0 then
      period = 0.2 * period + 0.8 * workMama[prev_base + P_PERIOD]
    end
    workMama[base + P_PERIOD] = period

    -- Phase
    local phase = (i1 ~= 0) and (math_atan(q1 / i1) * RAD2DEG) or 0.0
    workMama[base + P_PHASE] = phase

    -- 5. DeltaPhase & Alpha
    local delta_phase = (r == 0) and 0.0 or math_max(workMama[prev_base + P_PHASE] - phase, 1.0)
    local alpha = (delta_phase ~= 0) and math_max(math_min(inpFastLimit / delta_phase, inpFastLimit), inpSlowLimit) or
        1.0
    workMama[base + P_SA] = alpha

    -- 6. MAMA & FAMA Output
    local mama, fama
    if r == 0 then
      mama = price
      fama = price
    else
      mama = alpha * price + (1.0 - alpha) * workMama[prev_base + P_MAMA]
      fama = 0.5 * alpha * mama + (1.0 - 0.5 * alpha) * workMama[prev_base + P_FAMA]
    end

    workMama[base + P_MAMA] = mama
    workMama[base + P_FAMA] = fama

    valm[idx] = mama
    valf[idx] = fama
  end

  return valm, valf
end

function AVWAP(start)
  local cumVolume, cumPriceVolume, sumSqDiff = 0.0, 0.0, 0.0
  local avwapSeries = {}
  local upperBands, lowerBands = { {}, {}, {} }, { {}, {}, {} }

  if not start or start <= 0 or start > #WEIGHTED then start = 1 else start = #WEIGHTED - start end

  for i = start, #WEIGHTED do
    local price, volume = WEIGHTED[i], VOLUME[i]

    cumVolume = cumVolume + volume
    cumPriceVolume = cumPriceVolume + price * volume
    local avwap = cumPriceVolume / cumVolume

    sumSqDiff = sumSqDiff + volume * (price - avwap) * (price - avwap)
    local stdDev = math_sqrt(sumSqDiff / cumVolume)

    avwapSeries[#avwapSeries + 1] = avwap
    for k = 1, 3 do
      upperBands[k][#upperBands[k] + 1] = avwap + k * stdDev
      lowerBands[k][#lowerBands[k] + 1] = avwap - k * stdDev
    end
  end
  return avwapSeries, upperBands[1], lowerBands[1], upperBands[2], lowerBands[2], upperBands[3], lowerBands[3]
end

function DARVAS(highs, lows, boxp)
  -- Helper: lowest low over [i - length + 1 ... i]
  local function rolling_low(prices, length, i)
    local minv = prices[i]
    for j = i - length + 1, i do
      if j > 0 and prices[j] < minv then
        minv = prices[j]
      end
    end
    return minv
  end
  -- Helper: highest high over [i - length + 1 ... i]
  local function rolling_high(prices, length, i)
    local maxv = prices[i]
    for j = i - length + 1, i do
      if j > 0 and prices[j] > maxv then
        maxv = prices[j]
      end
    end
    return maxv
  end

  boxp = boxp or 5

  local TBox = {}
  local BBox = {}

  -- Array tracking historical state per bar
  local is_new_high_cond = {}
  local nh_array = {}

  local last_nh = nil
  local last_top_box = nil
  local last_bottom_box = nil

  -- Loop through all bars
  for i = boxp, #highs do
    -- 1. Calculate lowest low and highest highs
    local LL = rolling_low(lows, boxp, i)
    local k1_prev = rolling_high(highs, boxp, i - 1) -- k1[1]
    local k2 = rolling_high(highs, boxp - 1, i)      -- k2
    local k3 = rolling_high(highs, boxp - 2, i)      -- k3

    -- 2. high > k1[1] condition check
    local cond_nh = (highs[i] > k1_prev)
    is_new_high_cond[i] = cond_nh

    -- 3. NH = valuewhen(high > k1[1], high, 0)
    if cond_nh then
      last_nh = highs[i]
    end
    nh_array[i] = last_nh

    -- 4. Calculate barssince(high > k1[1])
    local barssince = nil
    for j = i, 1, -1 do
      if is_new_high_cond[j] then
        barssince = i - j
        break
      end
    end

    -- 5. box1 = k3 < k2
    local box1 = (k3 < k2)

    -- 6. Trigger condition: barssince(high > k1[1]) == boxp - 2 and box1
    local box_trigger = (barssince == (boxp - 2)) and box1

    -- 7. Update TopBox & BottomBox state
    if box_trigger and nh_array[i] ~= nil then
      last_top_box = nh_array[i]
      last_bottom_box = LL
    end

    table.insert(TBox, last_top_box)
    table.insert(BBox, last_bottom_box)
  end

  return TBox, BBox
end

function ALMA(prices, window, offset, sigma)
  prices = resolve_source(prices)
  local n = #prices
  local alma = {}
  local color = {}

  if n < window then
    return alma -- Not enough data points
  end

  -- 1. Precompute Gaussian Weights
  local m = offset * (window - 1)
  local s = window / sigma
  local weights = {}
  local norm = 0

  for i = 0, window - 1 do
    local weight = math.exp(-((i - m) ^ 2) / (2 * (s ^ 2)))
    weights[i + 1] = weight
    norm = norm + weight
  end

  -- 2. Calculate Weighted Moving Average across the price array
  color[1] = CLR_GREN
  for t = window, n do
    local sum = 0
    for i = 0, window - 1 do
      -- Apply precalculated weight (newest price gets weight[window])
      local price = prices[t - i]
      local weight = weights[window - i]
      sum = sum + price * weight
    end
    alma[t - window + 1] = sum / norm
    if (t ~= window and alma[t - window + 1] > alma[t - window + 1 - 1]) then
      color[t - window + 1] = CLR_GREEN
    else
      color[t - window + 1] =
          CLR_RED
    end
  end

  return alma, color
end

-- Portable unpack definition
local unpack = unpack or table.unpack or function(t, i, j)
  i = i or 1
  j = j or #t
  local res = {}
  for idx = i, j do
    res[#res + 1] = t[idx]
  end
  return table.unpack(res)
end

-- Helper function: Sum over N elements ending at bar index `end_idx`
local function sum(series, length, end_idx)
  end_idx = end_idx or #series
  if end_idx < length then return nil end
  local s = 0
  for i = 0, length - 1 do
    s = s + (series[end_idx - i] or 0)
  end
  return s
end

-- Helper function: Highest value over N periods ending at bar index `end_idx`
local function highest(series, length, end_idx)
  end_idx = end_idx or #series
  if end_idx < length then return nil end
  local max_val = series[end_idx]
  for i = 1, length - 1 do
    local val = series[end_idx - i]
    if val > max_val then max_val = val end
  end
  return max_val
end

-- Helper function: Lowest value over N periods ending at bar index `end_idx`
local function lowest(series, length, end_idx)
  end_idx = end_idx or #series
  if end_idx < length then return nil end
  local min_val = series[end_idx]
  for i = 1, length - 1 do
    local val = series[end_idx - i]
    if val < min_val then min_val = val end
  end
  return min_val
end

-- Helper function: Simple Moving Average (SMA) ending at bar index `end_idx`
local function sma(series, length, end_idx)
  end_idx = end_idx or #series
  if end_idx < length then return nil end
  local total = sum(series, length, end_idx)
  return total / length
end

-- Helper function: Williams %R ending at bar index `end_idx`
local function wpr(high_series, low_series, close_series, length, end_idx)
  end_idx = end_idx or #close_series
  if end_idx < length then return nil end

  local hh = highest(high_series, length, end_idx)
  local ll = lowest(low_series, length, end_idx)
  local current_close = close_series[end_idx]

  if not hh or not ll or hh == ll then return 0 end
  return ((hh - current_close) / (hh - ll)) * -100
end

--------------------------------------------------------------------------------
-- Main ASCTrend Calculation Function
--------------------------------------------------------------------------------

function ASCTrend(ASClength, RISK)
  local open, high, low, close, volume = OPEN, HIGH, LOW, CLOSE, VOLUME

  local eternal                        = 0
  local x1                             = 67 + RISK
  local x2                             = 33 - RISK

  -- Calculation buffers
  local range_series                   = {}
  local avg_range_series               = {}
  local count_fg_series                = {}
  local count_fg2_series               = {}
  local wpr_abs_series                 = {}
  local asc_trend_series               = {}
  local drawbuffer                     = {}
  local color                          = {}
  local avg_vol_series                 = {}

  local total_bars                     = #close

  for i = 1, total_bars do
    -- 1. Range = highest(ASClength) - lowest(ASClength)
    local h_val = highest(high, ASClength, i)
    local l_val = lowest(low, ASClength, i)
    local avg_vol = sma(volume, ASClength, i)
    avg_vol_series[i] = avg_vol or 0

    if h_val and l_val then
      range_series[i] = h_val - l_val
    else
      range_series[i] = 0
    end

    -- 2. AvgRange = sma(Range, ASClength)
    local avg_range     = sma(range_series, ASClength, i)
    avg_range_series[i] = avg_range or 0

    -- 3. CountFg = abs(open - close) >= AvgRange * 2.0 ? 1 : 0
    local curr_open     = open[i]
    local curr_close    = close[i]

    if avg_range and math.abs(curr_open - curr_close) >= (avg_range * 2.0) then
      count_fg_series[i] = 1
    else
      count_fg_series[i] = 0
    end

    -- 4. TrueCount = sum(CountFg, ASClength)
    local true_count = sum(count_fg_series, ASClength, i) or 0

    -- 5. CountFg2 = abs(close[3] - close) >= AvgRange * 4.6 ? 1 : 0
    if i > 3 and avg_range and math.abs(close[i - 3] - curr_close) >= (avg_range * 4.6) then
      count_fg2_series[i] = 1
    else
      count_fg2_series[i] = 0
    end

    -- 6. TrueCount2 = sum(CountFg2, ASClength - 3)
    local true_count2  = sum(count_fg2_series, math.max(1, ASClength - 3), i) or 0

    -- 7. Williams %R values
    local wpr3RR       = wpr(high, low, close, 3 + RISK + RISK, i) or 0
    local wpr3         = wpr(high, low, close, 3, i) or 0
    local wpr4         = wpr(high, low, close, 4, i) or 0

    -- 8. WprAbs = 100 + (TrueCount2 > 0 ? wpr4 : TrueCount > 0 ? wpr3 : wpr3RR)
    local selected_wpr = wpr3RR
    if true_count2 > 0 then
      selected_wpr = wpr4
    elseif true_count > 0 then
      selected_wpr = wpr3
    end

    wpr_abs_series[i] = 100 + selected_wpr

    -- 9. Trend State Determination & Synchronized Array Assignment
    local check_index = i - eternal

    if check_index < 1 then
      asc_trend_series[i] = 0
      drawbuffer[i] = close[i]
      color[i] = CLR_RED
    else
      local target_wpr_abs = wpr_abs_series[check_index]
      local prev_trend = (i > 1) and asc_trend_series[i - 1] or 0

      if target_wpr_abs < x2 then
        asc_trend_series[i] = -1
        drawbuffer[i] = h_val or high[i]
        if (volume[i] > avg_vol_series[i]) then color[i] = CLR_YELLOW else color[i] = CLR_RED end
      elseif target_wpr_abs > x1 then
        asc_trend_series[i] = 1
        drawbuffer[i] = l_val or low[i]
        if (volume[i] > avg_vol_series[i]) then color[i] = CLR_BLUE else color[i] = CLR_GREEN end
      else
        asc_trend_series[i] = prev_trend
        drawbuffer[i]       = (i > 1) and drawbuffer[i - 1] or close[i]
        color[i]            = (i > 1) and color[i - 1] or CLR_BLUE
      end
    end
  end

  return asc_trend_series, drawbuffer, color
end

-- Gaussian filter
function gaussian_filter(prices, length, sigma)
  -- Gaussian weights
  local function gaussian_weights(length, sigma)
    local weights, total = {}, 0.0
    local pi = math.pi
    for i = 1, length do
      local x = (i - length / 2) / sigma
      local w = math.exp(-0.5 * x * x) / math.sqrt(sigma * 2.0 * pi)
      weights[i] = w
      total = total + w
    end
    for i = 1, length do
      weights[i] = weights[i] / total
    end
    return weights
  end

  local weights = gaussian_weights(length, sigma)
  local smoothed = {}
  local color = {}



  for i = length, #prices do
    local sum = 0.0
    for j = 1, length do
      sum = sum + prices[i - j + 1] * weights[j]
    end
    table.insert(smoothed, sum)
    if i == length or (i ~= length and smoothed[#smoothed] > smoothed[#smoothed - 1]) then
      table.insert(color,
        CLR_GREEN)
    else
      table.insert(color, CLR_RED)
    end
  end
  return smoothed, color
end

local function SmoothedMA(position, period, prev_value, price)
  --
  local result = 0.0;
  -- check position
  if (period > 0) then
    if (position == period - 1) then
      for i = 0, period - 2, 1 do result = result + price[position - i]; end
      result = result / period;
    end
    if (position >= period) then
      result = (prev_value * (period - 1) + price[position]) / period;
    end
  end
  --
  return (result);
end

function ADXW(Period)
  local AdxW = {}
  local Pdi = {}
  local Mdi = {}
  local ExtPDSBuffer = {}
  local ExtNDSBuffer = {}
  local ExtPDBuffer = {}
  local ExtNDBuffer = {}
  local ExtTRBuffer = {}
  local ExtATRBuffer = {}
  local ExtDXBuffer = {}

  for i = 1, Period, 1 do
    AdxW[i] = 0
    Pdi[i] = 0
    Mdi[i] = 0
    ExtPDSBuffer[i] = 0
    ExtNDSBuffer[i] = 0
    ExtPDBuffer[i] = 0
    ExtNDBuffer[i] = 0
    ExtTRBuffer[i] = 0
    ExtATRBuffer[i] = 0
    ExtDXBuffer[i] = 0
  end
  -- main cycle
  for i = 2, #CLOSE, 1 do
    -- get some data
    local Hi     = HIGH[i]
    local prevHi = HIGH[i - 1]
    local Lo     = LOW[i]
    local prevLo = LOW[i - 1]
    local prevCl = CLOSE[i - 1]
    -- fill main positive and main negative buffers
    local dTmpP  = Hi - prevHi;
    local dTmpN  = prevLo - Lo;
    if (dTmpP < 0.0) then dTmpP = 0.0 end
    if (dTmpN < 0.0) then dTmpN = 0.0 end
    if (dTmpN == dTmpP) then
      dTmpN = 0.0;
      dTmpP = 0.0;
    else
      if (dTmpP < dTmpN) then
        dTmpP = 0.0
      else
        dTmpN = 0.0
      end
    end
    ExtPDBuffer[i] = dTmpP;
    ExtNDBuffer[i] = dTmpN;
    -- define TR
    local tr = math.max(math.max(math.abs(Hi - Lo), math.abs(Hi - prevCl)), math.abs(Lo - prevCl));
    -- write down TR to TR buffer
    ExtTRBuffer[i] = tr;
    -- fill smoothed positive and negative buffers and TR buffer
    if (i < Period) then
      ExtATRBuffer[i] = 0.0;
      Pdi[i] = 0.0;
      Mdi[i] = 0.0;
    else
      ExtATRBuffer[i] = SmoothedMA(i, Period, ExtATRBuffer[i - 1], ExtTRBuffer);
      ExtPDSBuffer[i] = SmoothedMA(i, Period, ExtPDSBuffer[i - 1], ExtPDBuffer);
      ExtNDSBuffer[i] = SmoothedMA(i, Period, ExtNDSBuffer[i - 1], ExtNDBuffer);
    end
    -- calculate PDI and NDI buffers
    if (ExtATRBuffer[i] ~= 0.0) then
      Pdi[i] = 100.0 * ExtPDSBuffer[i] / ExtATRBuffer[i];
      Mdi[i] = 100.0 * ExtNDSBuffer[i] / ExtATRBuffer[i];
    else
      Pdi[i] = 0.0;
      Mdi[i] = 0.0;
    end
    -- Calculate DX buffer
    local dTmp = Pdi[i] + Mdi[i];
    if (dTmp ~= 0.0) then
      dTmp = 100.0 * math.abs((Pdi[i] - Mdi[i]) / dTmp);
    else
      dTmp = 0.0
    end
    ExtDXBuffer[i] = dTmp;
    -- fill ADXW buffer as smoothed DX buffer
    AdxW[i] = SmoothedMA(i, Period, AdxW[i - 1], ExtDXBuffer);
  end
  return AdxW, Pdi, Mdi
end

-- Lua 5.1 Implementation using 4-tuples [x1, y1, x2, y2] for DRAW_TRENDLINE
function Trendlines(length, mult, calcMethod)
    local N = #CLOSE
    if N == 0 then
        return {}, {}, {}, {}
    end

    -- Output tables for DRAW_TRENDLINE store flat lists of 4-tuples:
    -- [x1_idx, y1_val, x2_idx, y2_val, x1_idx, y1_val, x2_idx, y2_val, ...]
    local upper_out = {}
    local lower_out = {}
    local up_break  = {}
    local dn_break  = {}

    for i = 1, N do
        up_break[i] = 0
        dn_break[i] = 0
    end

    -- O(N) Auxiliary Variables
    local tr        = {}
    local sum_tr    = 0.0
    local sum_src   = 0.0
    local sum_src2  = 0.0
    local sum_src_n = 0.0
    local var_n     = (length * length - 1) / 12.0

    -- State Variables for Upper Trendline
    local active_ph    = false
    local ph_start_bar = 0    -- 0-based bar index of active pivot high
    local ph_start_val = 0.0  -- Price value at pivot high
    local slope_ph     = 0.0

    -- State Variables for Lower Trendline
    local active_pl    = false
    local pl_start_bar = 0    -- 0-based bar index of active pivot low
    local pl_start_val = 0.0  -- Price value at pivot low
    local slope_pl     = 0.0

    local upos = 0
    local dnos = 0

    for n = 1, N do
        local src = CLOSE[n]

        -- 1. True Range (TR)
        if n == 1 then
            tr[n] = HIGH[1] - LOW[1]
        else
            local h_l  = HIGH[n] - LOW[n]
            local h_pc = math.abs(HIGH[n] - CLOSE[n - 1])
            local l_pc = math.abs(LOW[n] - CLOSE[n - 1])
            tr[n] = math.max(h_l, math.max(h_pc, l_pc))
        end

        -- 2. Sliding Window Sums
        sum_tr    = sum_tr + tr[n]
        sum_src   = sum_src + src
        sum_src2  = sum_src2 + (src * src)
        sum_src_n = sum_src_n + (src * n)

        if n > length then
            local prev_src = CLOSE[n - length]
            sum_tr    = sum_tr - tr[n - length]
            sum_src   = sum_src - prev_src
            sum_src2  = sum_src2 - (prev_src * prev_src)
            sum_src_n = sum_src_n - (prev_src * (n - length))
        end

        -- 3. Slope Calculation
        local slope = 0.0
        if n >= length then
            if calcMethod == 0 then
                local atr = sum_tr / length
                slope = (atr / length) * mult
            elseif calcMethod == 1 then
                local mean = sum_src / length
                local variance = (sum_src2 / length) - (mean * mean)
                if variance < 0 then variance = 0 end
                slope = (math.sqrt(variance) / length) * mult
            elseif calcMethod == 2 then
                local mean_src   = sum_src / length
                local mean_n     = n - (length - 1) / 2.0
                local mean_src_n = sum_src_n / length
                local cov        = mean_src_n - (mean_src * mean_n)
                local linreg_slope = cov / var_n
                slope = (math.abs(linreg_slope) / 2.0) * mult
            end
        end

        -- 4. Pivot High / Pivot Low Detection
        local ph = nil
        local pl = nil
        local pivot_idx = n - length

        if pivot_idx > length and pivot_idx + length <= N then
            local p_high = HIGH[pivot_idx]
            local p_low  = LOW[pivot_idx]
            local is_ph  = true
            local is_pl  = true

            for j = pivot_idx - length, pivot_idx + length do
                if j ~= pivot_idx then
                    if HIGH[j] >= p_high then is_ph = false end
                    if LOW[j]  <= p_low  then is_pl = false end
                end
            end

            if is_ph then ph = p_high end
            if is_pl then pl = p_low  end
        end

        -- 5. Update Upper Trendline Segments
        if ph then
            active_ph    = true
            slope_ph     = slope
            ph_start_bar = pivot_idx - 1  -- Convert 1-based index to 0-based bar index
            ph_start_val = ph

            -- Catch up segments from pivot_idx to current bar n
            for k = ph_start_bar + 1, n - 1 do
                local x1 = k - 1
                local y1 = ph_start_val - slope_ph * (x1 - ph_start_bar)
                local x2 = k
                local y2 = ph_start_val - slope_ph * (x2 - ph_start_bar)

                table.insert(upper_out, x1)
                table.insert(upper_out, y1)
                table.insert(upper_out, x2)
                table.insert(upper_out, y2)
            end
        elseif active_ph then
            local x1 = n - 2
            local y1 = ph_start_val - slope_ph * (x1 - ph_start_bar)
            local x2 = n - 1
            local y2 = ph_start_val - slope_ph * (x2 - ph_start_bar)

            table.insert(upper_out, x1)
            table.insert(upper_out, y1)
            table.insert(upper_out, x2)
            table.insert(upper_out, y2)
        end

        -- Projected Upper Level at bar n (0-based bar index: n - 1)
        local upper_level = 0.0
        if active_ph then
            upper_level = ph_start_val - slope_ph * ((n - 1) - ph_start_bar)
        end

        -- 6. Update Lower Trendline Segments
        if pl then
            active_pl    = true
            slope_pl     = slope
            pl_start_bar = pivot_idx - 1  -- Convert 1-based index to 0-based bar index
            pl_start_val = pl

            -- Catch up segments from pivot_idx to current bar n
            for k = pl_start_bar + 1, n - 1 do
                local x1 = k - 1
                local y1 = pl_start_val + slope_pl * (x1 - pl_start_bar)
                local x2 = k
                local y2 = pl_start_val + slope_pl * (x2 - pl_start_bar)

                table.insert(lower_out, x1)
                table.insert(lower_out, y1)
                table.insert(lower_out, x2)
                table.insert(lower_out, y2)
            end
        elseif active_pl then
            local x1 = n - 2
            local y1 = pl_start_val + slope_pl * (x1 - pl_start_bar)
            local x2 = n - 1
            local y2 = pl_start_val + slope_pl * (x2 - pl_start_bar)

            table.insert(lower_out, x1)
            table.insert(lower_out, y1)
            table.insert(lower_out, x2)
            table.insert(lower_out, y2)
        end

        -- Projected Lower Level at bar n (0-based bar index: n - 1)
        local lower_level = 0.0
        if active_pl then
            lower_level = pl_start_val + slope_pl * ((n - 1) - pl_start_bar)
        end

        -- 7. Breakout Tracking
        local prev_upos = upos
        local prev_dnos = dnos

        if ph then
            upos = 0
        elseif active_ph and CLOSE[n] > upper_level then
            upos = 1
        end

        if pl then
            dnos = 0
        elseif active_pl and CLOSE[n] < lower_level then
            dnos = 1
        end

        if upos > prev_upos then
            up_break[n] = LOW[n]
        end

        if dnos > prev_dnos then
            dn_break[n] = HIGH[n]
        end
    end

    return upper_out, lower_out, up_break, dn_break
end

-- Performs two-way crossover detection between series or numbers.
-- Automatically computes 'diff' internally using global/outer CLOSE array length.
function CROSS2(seriesA, seriesB)
  local buySignals = {}
  local sellSignals = {}

  -- Safely extract value at index 'idx' for either a table or a number
  local function getValue(src, idx)
    if type(src) == "table" then
      return src[idx]
    end
    return src
  end

  -- Automatically calculate internal offset using CLOSE
  local offset = 0
  if type(seriesA) == "table" and type(seriesB) == "table" then
    offset = #CLOSE - math.min(#seriesA, #seriesB)
  elseif type(seriesA) == "table" then
    offset = #CLOSE - #seriesA
  elseif type(seriesB) == "table" then
    offset = #CLOSE - #seriesB
  end


  -- Determine iteration length
  local len = 0
  if type(seriesA) == "table" and type(seriesB) == "table" then
    len = math.min(#seriesA, #seriesB)
  elseif type(seriesA) == "table" then
    len = #seriesA
  elseif type(seriesB) == "table" then
    len = #seriesB
  else
    return buySignals, sellSignals
  end

  for i = 2, len do
    local currA, prevA = getValue(seriesA, i), getValue(seriesA, i - 1)
    local currB, prevB = getValue(seriesB, i), getValue(seriesB, i - 1)

    -- Ensure values exist before comparing
    if currA and prevA and currB and prevB then
      -- Cross Above (Buy Signal)
      if currA > currB and prevA <= prevB then
        table.insert(buySignals, i + offset)
        -- Cross Below (Sell Signal)
      elseif currA < currB and prevA >= prevB then
        table.insert(sellSignals, i + offset)
      end
    end
  end

  return buySignals, sellSignals
end

-- Main conversion function
--   prc:          table of prices (1-indexed)
--   buysignals:   table of buy signal indices (1-indexed)
--   sellsignals:  table of sell signal indices (1-indexed)
-- returns a report table
function CreateTestReport(buysignals, sellsignals, ...)
  local report = { profit = 0, totalTrade = 0, profitableTrades = 0, buyLocs = {}, sellLocs = {}, opts = {} }
  local args = { ... }

  for i = 1, #args do
    for Index, Value in pairs(args[i]) do
      report["opts"][Index] = Value
    end
  end

  -- No signals
  if #buysignals == 0 and #sellsignals == 0 then
    return report
  end

  -- Only sell signals : insert buysignal at the first price
  if #buysignals == 0 then
    report.totalTrade = 1
    local buyprice = CLOSE[1]
    table.insert(report.buyLocs, 1)
    local sellprice = CLOSE[sellsignals[1]]
    table.insert(report.sellLocs, sellsignals[1])
    report.profit = (sellprice - buyprice) / buyprice * 100
    if buyprice < sellprice then
      report.profitableTrades = 1
    end
    return report
  end

  -- Only buy signals : insert sellsignal at the first price
  if #sellsignals == 0 then
    report.totalTrade = 1
    local buyprice = CLOSE[buysignals[1]]
    table.insert(report.buyLocs, buysignals[1])
    local lastIdx = #CLOSE
    local sellprice = CLOSE[lastIdx]
    table.insert(report.sellLocs, lastIdx)
    report.profit = (sellprice - buyprice) / buyprice * 100
    if buyprice < sellprice then
      report.profitableTrades = 1
    end
    return report
  end


  -- If the first signal is a sell, insert buysignal at the first price
  if buysignals[1] > sellsignals[1] then
    table.insert(buysignals, 1, 1) -- 1 = index of the first price
  end

  -- If the last signal is a buy, insert sellsignal at the last price
  if buysignals[#buysignals] > sellsignals[#sellsignals] then
    table.insert(sellsignals, #CLOSE)
  end

  -- Main pairing loop
  local lastpos = 0
  for i = 1, #buysignals do
    for j = 1, #sellsignals do
      if sellsignals[j] > buysignals[i] and buysignals[i] > lastpos then
        report.totalTrade = report.totalTrade + 1
        local buyprice = CLOSE[buysignals[i]]
        table.insert(report.buyLocs, buysignals[i])
        local sellprice = CLOSE[sellsignals[j]]
        table.insert(report.sellLocs, sellsignals[j])
        report.profit = report.profit + (sellprice - buyprice) / buyprice * 100
        if buyprice < sellprice then
          report.profitableTrades = report.profitableTrades + 1
        end
        lastpos = sellsignals[j]
        break -- pair found, move to next buy
      end
    end
  end

  return report
end

function DEBUG_RUN(asciidatafile, CALC)
  local i = 1
  for line in io.lines(asciidatafile) do
    local D, _T, O, H, L, C, V, M, T, W = line:match(
      "%s*(.-);%s*(.-);%s*(.-);%s*(.-);%s*(.+);%s*(.+);%s*(.+);%s*(.+);%s*(.+);%s*(.+)")
    DATE[i] = tostring(D)
    TIME[i] = tostring(_T)
    OPEN[i] = tonumber(O)
    HIGH[i] = tonumber(H)
    LOW[i] = tonumber(L)
    CLOSE[i] = tonumber(C)
    VOLUME[i] = tonumber(V)
    MEDIAN[i] = tonumber(M)
    TYPICAL[i] = tonumber(T)
    WEIGHTED[i] = tonumber(W)
    i = i + 1
  end

  CALC()


  --for member, member_value in pairs(REPORT) do
  --  print('\t', member, member_value)
  --  for idx, value in pairs(member_value) do
  --    print('\t', value)
  --  end
  --end
end
