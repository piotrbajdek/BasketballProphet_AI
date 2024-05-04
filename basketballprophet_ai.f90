! BSD 3-Clause No Military License
! Copyright © 2023-present, Piotr Bajdek

! gfortran -Ofast -march=native basketballprophet_ai.f90 -o basketballprophet_ai

program BasketballProphet_AI

  implicit none
  character(len=68) :: arg_1
  character(256) :: line
  character(len=3) :: st_h_0, st_l_0, st_h_1, st_l_1, st_sma_9
  integer, parameter :: dp = selected_real_kind(15, 307)
  integer, parameter :: qp = selected_real_kind(33, 4931)
  real(dp), dimension(:,:), allocatable :: attention_weights_6
  real(dp), dimension(:), allocatable :: sequence_1, sequence_2
  real(dp), dimension(:), allocatable :: sequence, sequence_3
  real(dp), dimension(6) :: w_6, dw_6
  real(dp) :: b_6 = 0.0_dp, alpha_6 = 0.001_dp, beta1_6 = 0.9_dp, beta2_6 = 0.999_dp, t_6 = 0.0_dp
  real(dp) :: mt_6(6) = 0.0_dp, vt_6(6) = 0.0_dp, m_hat_6(6), v_hat_6(6), denom_6(6)
  real(dp) :: db_6, y_pred_6, error_6
  real(dp) :: epsilon = 1.0E-8_dp
  integer :: i, j, iostat, seq_len, iterations = 12500, stop
  real :: result_1_6, result_2_6
  real(qp) :: ost_0, ost_1, ost_2, ost_3, ost_4, ost_5, ost_6, ost_7, ost_8, ost_9, ost_10, ost_11, ost_12, ost_13
  real(qp) :: ost_14, ost_15, ost_16, ost_17, ost_18
  real(qp) :: tr_0, tr_1, tr_2, tr_3, tr_4, tr_5, tr_6, tr_7
  real(qp) :: atr_2, atr_3, atr_4, atr_5, atr_6
  real(qp) :: atr_2_ex1, atr_3_ex1, atr_4_ex1, atr_5_ex1, atr_6_ex1
  real(qp) :: atr_2_ex2, atr_3_ex2, atr_4_ex2, atr_5_ex2, atr_6_ex2
  real(qp) :: rwi_h_2, rwi_h_3, rwi_h_4, rwi_h_5, rwi_h_6
  real(qp) :: rwi_l_2, rwi_l_3, rwi_l_4, rwi_l_5, rwi_l_6
  real(qp) :: rwi_h_2_ex1, rwi_h_3_ex1, rwi_h_4_ex1, rwi_h_5_ex1, rwi_h_6_ex1, rwi_h_7_ex1, rwi_h_8_ex1, rwi_h_9_ex1
  real(qp) :: rwi_l_2_ex1, rwi_l_3_ex1, rwi_l_4_ex1, rwi_l_5_ex1, rwi_l_6_ex1, rwi_l_7_ex1, rwi_l_8_ex1, rwi_l_9_ex1
  real(qp) :: rwi_h_2_ex2, rwi_h_3_ex2, rwi_h_4_ex2, rwi_h_5_ex2, rwi_h_6_ex2, rwi_h_7_ex2, rwi_h_8_ex2, rwi_h_9_ex2
  real(qp) :: rwi_l_2_ex2, rwi_l_3_ex2, rwi_l_4_ex2, rwi_l_5_ex2, rwi_l_6_ex2, rwi_l_7_ex2, rwi_l_8_ex2, rwi_l_9_ex2
  real(qp) :: rwi_h_6_0, rwi_l_6_0, rwi_h_6_1, rwi_l_6_1, rwi_h_6_2, rwi_l_6_2
  real(dp) :: high_4, low_4, high_4_1, low_4_1, high_4_2, low_4_2, so_4
  real(dp) :: high_6, low_6, high_6_1, low_6_1, high_6_2, low_6_2, so_6, k_0, k_1, k_2
  real(dp) :: aad_9, sma_9_0, sma_9_1, band_u, band_l, sma_13_0, sma_19_0
  real(dp) :: v_0, v_1, v_2, v_3, v_4, v_5, v_6, v_7, v_8

  character(len=4) :: reset = ''//achar(27)//'[0m'
  character(len=5) :: red = ''//achar(27)//'[31m'
  character(len=5) :: green = ''//achar(27)//'[32m'
  character(len=5) :: brown = ''//achar(27)//'[33m'
  character(len=11) :: grey = ''//achar(27)//'[38;5;246m'

  character(len=5) :: a_col_6, b_col_6, kol_h_0, kol_l_0, kol_h_1, kol_l_1, kol_so_4, kol_so_6, kol_vol_u, kol_vol_l
  character(len=5) :: kol_st_sma_9

  if (command_argument_count() == 0) then
    write(*,'(a)') 'No input file selected!'
    call exit(0)
  end if

  call get_command_argument(1, arg_1)

  open(10, file=arg_1, status='old', action='read')

  seq_len = 0
  do
    read(10, *, iostat=i)
    if (i /= 0) exit
    seq_len = seq_len + 1
  end do

  allocate(sequence_1(seq_len))
  allocate(sequence_2(seq_len))
  allocate(sequence_3(seq_len))

  rewind(10)

  do i = 1, seq_len
    read(10, '(A)', iostat=iostat) line
    if (iostat /= 0) then
      print *, "Read error in line ", i
      exit
    end if
    read(line, *) sequence_1(i), sequence_2(i)
  end do

  close(10)

  if (seq_len < 24) then
    write(*,'(a)') 'The sequence must have a minimum length of 24 records!'
    call exit(1)
  end if

  sequence = sequence_1
    call ai()
  result_1_6 = y_pred_6
    deallocate(attention_weights_6)

  sequence = sequence_2
    call ai()
  result_2_6 = y_pred_6
    deallocate(attention_weights_6)

  if (result_1_6 > result_2_6) then
    a_col_6 = green
    b_col_6 = red
  else if (result_1_6 < result_2_6) then
    a_col_6 = red
    b_col_6 = green
  else
    a_col_6 = brown
    b_col_6 = brown
  end if

sequence_3 = sequence_1 / sequence_2

  deallocate(sequence_1, sequence_2)

    call rwi()

  deallocate(sequence_3)

  write(*,'(a)') 'Neural Network-Based Sport Predictions'
  write(*,'(a,a,a,a,a)') grey, 'BasketballProphet AI', reset, ' v0.6.0 04.05.2024'
  write(*,'(a)') 'Copyright © 2023-present, Piotr Bajdek'
  print *,''
  write(*,'(a,a,a,a)') 'Loaded file ', grey, arg_1, reset
  write(*,'(a,a,a,a,a,a,a,a)') '                   ', a_col_6, 'A', reset, '     ', b_col_6, 'B', reset
  write(*,'(a,a,I4,a,a,a,I4,a,a)') 'Predicted score:', a_col_6, nint(result_1_6), reset, ' :', b_col_6, nint(result_2_6), reset, &
  ' (6 neurons)'

 if (rwi_h_6_0 > rwi_h_6_1) then
  kol_h_0 = green
  st_h_0 = '↑'
else if (rwi_h_6_0 < rwi_h_6_1) then
  kol_h_0 = red
  st_h_0 = '↓'
else
  kol_h_0 = brown
  st_h_0 = '↕'
end if

if (rwi_l_6_0 > rwi_l_6_1) then
  kol_l_0 = green
  st_l_0 = '↑'
else if (rwi_l_6_0 < rwi_l_6_1) then
  kol_l_0 = red
  st_l_0 = '↓'
else
  kol_l_0 = brown
  st_l_0 = '↕'
end if

if (rwi_h_6_1 > rwi_h_6_2) then
  kol_h_1 = green
  st_h_1 = '↑'
else if (rwi_h_6_1 < rwi_h_6_2) then
  kol_h_1 = red
  st_h_1 = '↓'
else
  kol_h_1 = brown
  st_h_1 = '↕'
end if

if (rwi_l_6_1 > rwi_l_6_2) then
  kol_l_1 = green
  st_l_1 = '↑'
else if (rwi_l_6_1 < rwi_l_6_2) then
  kol_l_1 = red
  st_l_1 = '↓'
else
  kol_l_1 = brown
  st_l_1 = '↕'
end if

  print *,''

    call bands()

if (ost_0 >= band_u) then
  kol_vol_u = red
else if (ost_0 + atr_4 < band_u) then
  kol_vol_u = green
else
  kol_vol_u = brown
end if

if (ost_0 < band_l) then
  kol_vol_l = green
else if (ost_0 - atr_4 > band_l) then
  kol_vol_l = red
else
  kol_vol_l = brown
end if

if (ost_0 >= 1.0) then
  write(*,'(a,F0.3,a,a,F0.2,a,a,a,a,F0.2,a)') 'A/B ratio: ', ost_0, ' Volat. Bs: ', &
  kol_vol_u, band_u, reset, ', ', kol_vol_l, '0', band_l, reset
else
  write(*,'(a,F0.3,a,a,F0.2,a,a,a,a,F0.2,a)') 'A/B ratio: 0', ost_0, ' Volat. Bs: ', &
  kol_vol_u, band_u, reset, ', ', kol_vol_l, '0', band_l, reset
end if

  print *,''

if (sma_9_0 > sma_9_1) then
  kol_st_sma_9 = green
  st_sma_9 = '↑'
else if (sma_9_0 < sma_9_1) then
  kol_st_sma_9 = red
  st_sma_9 = '↓'
else
  kol_st_sma_9 = brown
  st_sma_9 = '↕'
end if

if (sma_9_0 < 1.0 .and. sma_13_0 < 1.0 .and. sma_19_0 < 1.0) then
  write(*,'(a,F0.3,a,a,a,a,F0.3,a,F0.3)') 'MA 9: 0', sma_9_0, kol_st_sma_9, st_sma_9, reset, ' MA 13: 0', sma_13_0, ' MA 19: 0', sma_19_0
else if (sma_9_0 >= 1.0 .and. sma_13_0 < 1.0 .and. sma_19_0 < 1.0) then
  write(*,'(a,F0.3,a,a,a,a,F0.3,a,F0.3)') 'MA 9: ', sma_9_0, kol_st_sma_9, st_sma_9, reset, ' MA 13: 0', sma_13_0, ' MA 19: 0', sma_19_0
else if (sma_9_0 < 1.0 .and. sma_13_0 >= 1.0 .and. sma_19_0 < 1.0) then
  write(*,'(a,F0.3,a,a,a,a,F0.3,a,F0.3)') 'MA 9: 0', sma_9_0, kol_st_sma_9, st_sma_9, reset, ' MA 13: ', sma_13_0, ' MA 19: 0', sma_19_0
else if (sma_9_0 < 1.0 .and. sma_13_0 < 1.0 .and. sma_19_0 >= 1.0) then
  write(*,'(a,F0.3,a,a,a,a,F0.3,a,F0.3)') 'MA 9: 0', sma_9_0, kol_st_sma_9, st_sma_9, reset, ' MA 13: 0', sma_13_0, ' MA 19: ', sma_19_0
else if (sma_9_0 >= 1.0 .and. sma_13_0 >= 1.0 .and. sma_19_0 < 1.0) then
  write(*,'(a,F0.3,a,a,a,a,F0.3,a,F0.3)') 'MA 9: ', sma_9_0, kol_st_sma_9, st_sma_9, reset, ' MA 13: ', sma_13_0, ' MA 19: 0', sma_19_0
else if (sma_9_0 >= 1.0 .and. sma_13_0 < 1.0 .and. sma_19_0 >= 1.0) then
  write(*,'(a,F0.3,a,a,a,a,F0.3,a,F0.3)') 'MA 9: ', sma_9_0, kol_st_sma_9, st_sma_9, reset, ' MA 13: 0', sma_13_0, ' MA 19: ', sma_19_0
else if (sma_9_0 < 1.0 .and. sma_13_0 >= 1.0 .and. sma_19_0 >= 1.0) then
  write(*,'(a,F0.3,a,a,a,a,F0.3,a,F0.3)') 'MA 9: 0', sma_9_0, kol_st_sma_9, st_sma_9, reset, ' MA 13: ', sma_13_0, ' MA 19: ', sma_19_0
else if (sma_9_0 >= 1.0 .and. sma_13_0 >= 1.0 .and. sma_19_0 >= 1.0) then
  write(*,'(a,F0.3,a,a,a,a,F0.3,a,F0.3)') 'MA 9: ', sma_9_0, kol_st_sma_9, st_sma_9, reset, ' MA 13: ', sma_13_0, ' MA 19: ', sma_19_0
end if

  print *,''

if (rwi_h_6_0 <= -1.0) then
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', green, 'H', reset, ' (present):', rwi_h_6_0, '  ', kol_h_0, st_h_0, reset
else if (rwi_h_6_0 > 0.0 .and. rwi_h_6_0 < 1.0) then
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', green, 'H', reset, ' (present): 0', rwi_h_6_0, '  ', kol_h_0, st_h_0, reset
else
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', green, 'H', reset, ' (present): ', rwi_h_6_0, '  ', kol_h_0, st_h_0, reset
end if

if (rwi_l_6_0 <= -1.0) then
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', red, 'L', reset, ' (present):', rwi_l_6_0, '  ', kol_l_0, st_l_0, reset
else if (rwi_l_6_0 > 0.0 .and. rwi_l_6_0 < 1.0) then
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', red, 'L', reset, ' (present): 0', rwi_l_6_0, '  ', kol_l_0, st_l_0, reset
else
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', red, 'L', reset, ' (present): ', rwi_l_6_0, '  ', kol_l_0, st_l_0, reset
end if

if (rwi_h_6_1 <= -1.0) then
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', green, 'H', reset, ' (game -1):', rwi_h_6_1, '  ',  kol_h_1, st_h_1, reset
else if (rwi_h_6_1 > 0.0 .and. rwi_h_6_1 < 1.0) then
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', green, 'H', reset, ' (game -1): 0', rwi_h_6_1, '  ',  kol_h_1, st_h_1, reset
else
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', green, 'H', reset, ' (game -1): ', rwi_h_6_1, '  ',  kol_h_1, st_h_1, reset
end if

if (rwi_l_6_1 <= -1.0) then
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', red, 'L', reset, ' (game -1):', rwi_l_6_1, '  ', kol_l_1, st_l_1, reset
else if (rwi_l_6_1 > 0.0 .and. rwi_l_6_1 < 1.0) then
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', red, 'L', reset, ' (game -1): 0', rwi_l_6_1, '  ', kol_l_1, st_l_1, reset
else
  write(*,'(a,a,a,a,a,F0.5,a,a,a,a)') 'Team A/B RWI 6 ', red, 'L', reset, ' (game -1): ', rwi_l_6_1, '  ', kol_l_1, st_l_1, reset
end if

if (rwi_h_6_2 <= -1.0) then
  write(*,'(a,a,a,a,a,F0.5)') 'Team A/B RWI 6 ', green, 'H', reset, ' (game -2):', rwi_h_6_2
else if (rwi_h_6_2 > 0.0 .and. rwi_h_6_2 < 1.0) then
  write(*,'(a,a,a,a,a,F0.5)') 'Team A/B RWI 6 ', green, 'H', reset, ' (game -2): 0', rwi_h_6_2
else
  write(*,'(a,a,a,a,a,F0.5)') 'Team A/B RWI 6 ', green, 'H', reset, ' (game -2): ', rwi_h_6_2
end if

if (rwi_l_6_2 <= -1.0) then
  write(*,'(a,a,a,a,a,F0.5)') 'Team A/B RWI 6 ', red, 'L', reset, ' (game -2):', rwi_l_6_2
else if (rwi_l_6_2 > 0.0 .and. rwi_l_6_2 < 1.0) then
  write(*,'(a,a,a,a,a,F0.5)') 'Team A/B RWI 6 ', red, 'L', reset, ' (game -2): 0', rwi_l_6_2
else
  write(*,'(a,a,a,a,a,F0.5)') 'Team A/B RWI 6 ', red, 'L', reset, ' (game -2): ', rwi_l_6_2
end if

    call so()

  print *,''

if (so_6 < 50.0) then
  kol_so_6 = green
else if (so_6 > 50.0) then
  kol_so_6 = red
else
  kol_so_6 = brown
end if

if (so_4 < 50.0) then
  kol_so_4 = green
else if (so_4 > 50.0) then
  kol_so_4 = red
else
  kol_so_4 = brown
end if

if (so_6 == 0.0) then
  write(*,'(a,a,a,F0.5,a,a)') 'Stochastic oscillator 6(3): ', kol_so_6, '0', so_6, reset, ' 🏀'
else if (so_6 == 100.0) then
  write(*,'(a,a,F0.3,a,a)') 'Stochastic oscillator 6(3): ', kol_so_6, so_6, reset, ' 🏀'
else if (so_6 < 10.0) then
  write(*,'(a,a,F0.5,a,a)') 'Stochastic oscillator 6(3): ', kol_so_6, so_6, reset, ' 🏀'
else
  write(*,'(a,a,F0.4,a,a)') 'Stochastic oscillator 6(3): ', kol_so_6, so_6, reset, ' 🏀'
end if

if (so_4 == 0.0) then
  write(*,'(a,a,a,F0.5,a,a)') 'Stochastic oscillator 4(3): ', kol_so_4, '0', so_4, reset, ' 🏀'
else if (so_4 == 100.0) then
  write(*,'(a,a,F0.3,a,a)') 'Stochastic oscillator 4(3): ', kol_so_4, so_4, reset, ' 🏀'
else if (so_4 < 10.0) then
  write(*,'(a,a,F0.5,a,a)') 'Stochastic oscillator 4(3): ', kol_so_4, so_4, reset, ' 🏀'
else
  write(*,'(a,a,F0.4,a,a)') 'Stochastic oscillator 4(3): ', kol_so_4, so_4, reset, ' 🏀'
end if

  print *,''

if (sma_9_0 < sma_9_1 .and. ost_0 < sma_13_0 .and. ost_0 < 1.0 .and. ost_0 > 0.925 .and. &
    sma_9_0 < sma_13_0 .and. sma_13_0 < sma_19_0) then
  stop = 1
else if (sma_9_0 > sma_9_1 .and. ost_0 > sma_13_0 .and. ost_0 > 1.0 .and. ost_0 < 1.075 .and. &
    sma_9_0 > sma_13_0 .and. sma_13_0 > sma_19_0) then
  stop = -1
else
  stop = 0
end if

if ((result_1_6 > result_2_6 .and. stop <= 0 .and. &
    (so_4 < 50.0 .or. rwi_h_6_0 > rwi_l_6_0 .or. ost_0 < 0.925) .and. &
    (so_6 < 50.0 .or. rwi_h_6_0 > 1.0 .or. rwi_l_6_0 < 1.0) .and. &
    so_4 < 100.0 .and. so_6 < 85.0 .and. ost_0 < band_u .and. &
    (so_6 < 60 .or. rwi_h_6_0 > 1.0) .and. &
    (so_6 < 50 .or. rwi_h_6_1 > 0.5 .or. rwi_h_6_2 > 0.7) .and. &
    (rwi_h_6_2 > 0.0 .or. rwi_h_6_1 > 1.0) .and. &
    rwi_h_6_0 > -0.5 .and. rwi_h_6_1 > -0.5 .and. &
    (ost_0 < 1.06 .or. rwi_h_6_0 > rwi_l_6_0)) &
    .or. &
    ((ost_0 < band_l .or. (so_4 < 15.0 .and. so_6 < 15.0) .or. so_4 < 1.0) .and. &
    rwi_h_6_0 < 0.0 .and. rwi_l_6_0 > 1.0 .and. so_6 < 30.0)) then
  write(*,'(a,a,a,a,a)') 'Predicted winner: Team ', green, 'A', reset, ' >90% prob. 🏆'
else if ((result_1_6 < result_2_6 .and. stop >= 0 .and. &
    (so_4 > 50.0 .or. rwi_l_6_0 > rwi_h_6_0 .or. ost_0 > 1.075) .and. &
    (so_6 > 50.0 .or. rwi_l_6_0 > 1.0 .or. rwi_h_6_0 < 1.0) .and. &
    so_4 > 0.0 .and. so_6 > 15.0 .and. ost_0 > band_l .and. &
    (so_6 > 40.0 .or. rwi_l_6_0 > 1.0) .and. &
    (so_6 > 50 .or. rwi_l_6_1 > 0.5 .or. rwi_l_6_2 > 0.7) .and. &
    (rwi_l_6_2 > 0.0 .or. rwi_l_6_1 > 1.0) .and. &
    rwi_l_6_0 > -0.5 .and. rwi_l_6_1 > -0.5 .and. &
    (ost_0 > 0.94 .or. rwi_l_6_0 > rwi_h_6_0)) &
    .or. &
    ((ost_0 > band_u .or. (so_4 > 85.0 .and. so_6 > 85.0) .or. so_4 > 99.0) .and. &
    rwi_l_6_0 < 0.0 .and. rwi_h_6_0 > 1.0 .and. so_6 > 70.0)) then
  write(*,'(a,a,a,a,a)') 'Predicted winner: Team ', green, 'B', reset, ' >90% prob. 🏆'
else
  write(*,'(a)') 'Predicted winner: uncertain'
end if

contains

  subroutine ai()

  b_6 = 0.0_dp
  db_6 = 0.0_dp
  w_6 = 0.0_dp
  t_6 = 0.0_dp
  mt_6 = 0.0_dp
  t_6 = 0.0_dp

  allocate(attention_weights_6(seq_len, seq_len))
  attention_weights_6 = 0.0_dp

  do i=1, iterations

      dw_6 = 0.0_dp

      do j = 1, seq_len
        attention_weights_6(:, j) = sequence(j) * sequence(:)
      end do

      attention_weights_6 = attention_weights_6 / real(seq_len)

      do j=7, seq_len
          y_pred_6 = w_6(1) * sequence(j-2) &
          + w_6(2) * sequence(j-3) &
          + w_6(3) * sequence(j-4) &
          + w_6(4) * sequence(j-5) &
          + w_6(5) * sequence(j-6) &
          + w_6(6) * sequence(j-7) &
          + b_6
          error_6 = y_pred_6 - sequence(j)
          dw_6(1) = dw_6(1) + error_6 * sequence(j-2) * attention_weights_6(j-2, j)
          dw_6(2) = dw_6(2) + error_6 * sequence(j-3) * attention_weights_6(j-3, j)
          dw_6(3) = dw_6(3) + error_6 * sequence(j-4) * attention_weights_6(j-4, j)
          dw_6(4) = dw_6(4) + error_6 * sequence(j-5) * attention_weights_6(j-5, j)
          dw_6(5) = dw_6(5) + error_6 * sequence(j-6) * attention_weights_6(j-6, j)
          dw_6(6) = dw_6(6) + error_6 * sequence(j-7) * attention_weights_6(j-7, j)
          db_6 = db_6 + error_6
      end do

      dw_6 = dw_6 / (seq_len - 6)
      db_6 = db_6 / (seq_len - 6)

      t_6 = t_6 + 1.0_dp
      mt_6 = beta1_6 * mt_6 + (1.0_dp - beta1_6) * dw_6
      vt_6 = beta2_6 * vt_6 + (1.0_dp - beta2_6) * (dw_6**2)
      m_hat_6 = mt_6 / (1.0_dp - beta1_6**t_6)
      v_hat_6 = vt_6 / (1.0_dp - beta2_6**t_6)
      denom_6 = sqrt(v_hat_6) + epsilon
      w_6 = w_6 - alpha_6 * m_hat_6 / denom_6
      b_6 = b_6 - alpha_6 * db_6

  end do

  y_pred_6 = w_6(1) * sequence(seq_len-2) &
      + w_6(2) * sequence(seq_len-3) &
      + w_6(3) * sequence(seq_len-4) &
      + w_6(4) * sequence(seq_len-5) &
      + w_6(5) * sequence(seq_len-6) &
      + w_6(6) * sequence(seq_len-7) &
      + b_6

  end subroutine ai

  subroutine rwi()

ost_0 = sequence_3(seq_len)
ost_1 = sequence_3(seq_len-1)
ost_2 = sequence_3(seq_len-2)
ost_3 = sequence_3(seq_len-3)
ost_4 = sequence_3(seq_len-4)
ost_5 = sequence_3(seq_len-5)
ost_6 = sequence_3(seq_len-6)
ost_7 = sequence_3(seq_len-7)
ost_8 = sequence_3(seq_len-8)
ost_9 = sequence_3(seq_len-9)
ost_10 = sequence_3(seq_len-10)
ost_11 = sequence_3(seq_len-11)
ost_12 = sequence_3(seq_len-12)
ost_13 = sequence_3(seq_len-13)
ost_14 = sequence_3(seq_len-14)
ost_15 = sequence_3(seq_len-15)
ost_16 = sequence_3(seq_len-16)
ost_17 = sequence_3(seq_len-17)
ost_18 = sequence_3(seq_len-18)

tr_0 = abs(ost_0 - ost_1)
tr_1 = abs(ost_1 - ost_2)
tr_2 = abs(ost_2 - ost_3)
tr_3 = abs(ost_3 - ost_4)
tr_4 = abs(ost_4 - ost_5)
tr_5 = abs(ost_5 - ost_6)
tr_6 = abs(ost_6 - ost_7)
tr_7 = abs(ost_7 - ost_8)

atr_2 = (tr_0 + tr_1) / 2
atr_3 = (tr_0 + tr_1 + tr_2) / 3
atr_4 = (tr_0 + tr_1 + tr_2 + tr_3) / 4
atr_5 = (tr_0 + tr_1 + tr_2 + tr_3 + tr_4) / 5
atr_6 = (tr_0 + tr_1 + tr_2 + tr_3 + tr_4 + tr_5) / 6

atr_2_ex1 = (tr_1 + tr_2) / 2
atr_3_ex1 = (tr_1 + tr_2 + tr_3) / 3
atr_4_ex1 = (tr_1 + tr_2 + tr_3 + tr_4) / 4
atr_5_ex1 = (tr_1 + tr_2 + tr_3 + tr_4 + tr_5) / 5
atr_6_ex1 = (tr_1 + tr_2 + tr_3 + tr_4 + tr_5 + tr_6) / 6

atr_2_ex2 = (tr_2 + tr_3) / 2
atr_3_ex2 = (tr_2 + tr_3 + tr_4) / 3
atr_4_ex2 = (tr_2 + tr_3 + tr_4 + tr_5) / 4
atr_5_ex2 = (tr_2 + tr_3 + tr_4 + tr_5 + tr_6) / 5
atr_6_ex2 = (tr_2 + tr_3 + tr_4 + tr_5 + tr_6 + tr_7) / 6

rwi_h_2 = (ost_0 - ost_1) / (atr_2 * sqrt(2.0))
rwi_h_3 = (ost_0 - ost_2) / (atr_3 * sqrt(3.0))
rwi_h_4 = (ost_0 - ost_3) / (atr_4 * sqrt(4.0))
rwi_h_5 = (ost_0 - ost_4) / (atr_5 * sqrt(5.0))
rwi_h_6 = (ost_0 - ost_5) / (atr_6 * sqrt(6.0))

rwi_l_2 = (ost_1 - ost_0) / (atr_2 * sqrt(2.0))
rwi_l_3 = (ost_2 - ost_0) / (atr_3 * sqrt(3.0))
rwi_l_4 = (ost_3 - ost_0) / (atr_4 * sqrt(4.0))
rwi_l_5 = (ost_4 - ost_0) / (atr_5 * sqrt(5.0))
rwi_l_6 = (ost_5 - ost_0) / (atr_6 * sqrt(6.0))

rwi_h_2_ex1 = (ost_1 - ost_2) / (atr_2_ex1 * sqrt(2.0))
rwi_h_3_ex1 = (ost_1 - ost_3) / (atr_3_ex1 * sqrt(3.0))
rwi_h_4_ex1 = (ost_1 - ost_4) / (atr_4_ex1 * sqrt(4.0))
rwi_h_5_ex1 = (ost_1 - ost_5) / (atr_5_ex1 * sqrt(5.0))
rwi_h_6_ex1 = (ost_1 - ost_6) / (atr_6_ex1 * sqrt(6.0))

rwi_l_2_ex1 = (ost_2 - ost_1) / (atr_2_ex1 * sqrt(2.0))
rwi_l_3_ex1 = (ost_3 - ost_1) / (atr_3_ex1 * sqrt(3.0))
rwi_l_4_ex1 = (ost_4 - ost_1) / (atr_4_ex1 * sqrt(4.0))
rwi_l_5_ex1 = (ost_5 - ost_1) / (atr_5_ex1 * sqrt(5.0))
rwi_l_6_ex1 = (ost_6 - ost_1) / (atr_6_ex1 * sqrt(6.0))

rwi_h_2_ex2 = (ost_2 - ost_3) / (atr_2_ex2 * sqrt(2.0))
rwi_h_3_ex2 = (ost_2 - ost_4) / (atr_3_ex2 * sqrt(3.0))
rwi_h_4_ex2 = (ost_2 - ost_5) / (atr_4_ex2 * sqrt(4.0))
rwi_h_5_ex2 = (ost_2 - ost_6) / (atr_5_ex2 * sqrt(5.0))
rwi_h_6_ex2 = (ost_2 - ost_7) / (atr_6_ex2 * sqrt(6.0))

rwi_l_2_ex2 = (ost_3 - ost_2) / (atr_2_ex2 * sqrt(2.0))
rwi_l_3_ex2 = (ost_4 - ost_2) / (atr_3_ex2 * sqrt(3.0))
rwi_l_4_ex2 = (ost_5 - ost_2) / (atr_4_ex2 * sqrt(4.0))
rwi_l_5_ex2 = (ost_6 - ost_2) / (atr_5_ex2 * sqrt(5.0))
rwi_l_6_ex2 = (ost_7 - ost_2) / (atr_6_ex2 * sqrt(6.0))

rwi_h_6_0 = max(rwi_h_2, rwi_h_3, rwi_h_4, rwi_h_5, rwi_h_6)
rwi_l_6_0 = max(rwi_l_2, rwi_l_3, rwi_l_4, rwi_l_5, rwi_l_6)

rwi_h_6_1 = max(rwi_h_2_ex1, rwi_h_3_ex1, rwi_h_4_ex1, rwi_h_5_ex1, rwi_h_6_ex1)
rwi_l_6_1 = max(rwi_l_2_ex1, rwi_l_3_ex1, rwi_l_4_ex1, rwi_l_5_ex1, rwi_l_6_ex1)

rwi_h_6_2 = max(rwi_h_2_ex2, rwi_h_3_ex2, rwi_h_4_ex2, rwi_h_5_ex2, rwi_h_6_ex2)
rwi_l_6_2 = max(rwi_l_2_ex2, rwi_l_3_ex2, rwi_l_4_ex2, rwi_l_5_ex2, rwi_l_6_ex2)

  end subroutine rwi

  subroutine so()


high_4 = max(ost_0, ost_1, ost_2, ost_3)
low_4 = min(ost_0, ost_1, ost_2, ost_3)

high_4_1 = max(ost_1, ost_2, ost_3, ost_4)
low_4_1 = min(ost_1, ost_2, ost_3, ost_4)
 
high_4_2 = max(ost_2, ost_3, ost_4, ost_5)
low_4_2 = min(ost_2, ost_3, ost_4, ost_5)

k_0 = (ost_0 - low_4) / (high_4 - low_4) * 100
k_1 = (ost_1 - low_4_1) / (high_4_1 - low_4_1) * 100
k_2 = (ost_2 - low_4_2) / (high_4_2 - low_4_2) * 100

so_4 = (k_0 + k_1 + k_2) / 3

high_6 = max(ost_0, ost_1, ost_2, ost_3, ost_4, ost_5)
low_6 = min(ost_0, ost_1, ost_2, ost_3, ost_4, ost_5)

high_6_1 = max(ost_1, ost_2, ost_3, ost_4, ost_5, ost_6)
low_6_1 = min(ost_1, ost_2, ost_3, ost_4, ost_5, ost_6)
 
high_6_2 = max(ost_2, ost_3, ost_4, ost_5, ost_6, ost_7)
low_6_2 = min(ost_2, ost_3, ost_4, ost_5, ost_6, ost_7)

k_0 = (ost_0 - low_6) / (high_6 - low_6) * 100
k_1 = (ost_1 - low_6_1) / (high_6_1 - low_6_1) * 100
k_2 = (ost_2 - low_6_2) / (high_6_2 - low_6_2) * 100

so_6 = (k_0 + k_1 + k_2) / 3

  end subroutine so

  subroutine bands()

sma_9_0 = (ost_0 + ost_1 + ost_2 + ost_3 + ost_4 + ost_5 + ost_6 + ost_7 + ost_8) / 9
sma_9_1 = (ost_1 + ost_2 + ost_3 + ost_4 + ost_5 + ost_6 + ost_7 + ost_8 + ost_9) / 9

sma_13_0 = (ost_0 + ost_1 + ost_2 + ost_3 + ost_4 + ost_5 + ost_6 + ost_7 + ost_8 &
         + ost_9 + ost_10 + ost_11 + ost_12) / 13

sma_19_0 = (ost_0 + ost_1 + ost_2 + ost_3 + ost_4 + ost_5 + ost_6 + ost_7 + ost_8 &
         + ost_9 + ost_10 + ost_11 + ost_12 + ost_13 + ost_14 + ost_15 + ost_16 &
         + ost_17 + ost_18) / 19

v_0 = abs(sma_9_0 - ost_0)
v_1 = abs(sma_9_0 - ost_1)
v_2 = abs(sma_9_0 - ost_2)
v_3 = abs(sma_9_0 - ost_3)
v_4 = abs(sma_9_0 - ost_4)
v_5 = abs(sma_9_0 - ost_5)
v_6 = abs(sma_9_0 - ost_6)
v_7 = abs(sma_9_0 - ost_7)
v_8 = abs(sma_9_0 - ost_8)

aad_9 = (v_0 + v_1 + v_2 + v_3 + v_4 + v_5 + v_6 + v_7 + v_8) / 9

if (sma_9_0 <= sma_9_1) then
    band_u = sma_9_0 + (aad_9 * 1.46557)
else
    band_u = sma_9_0 + (aad_9 * 2.46557)
end if

if (sma_9_0 >= sma_9_1) then
    band_l = sma_9_0 - (aad_9 * 1.46557)
else
    band_l = sma_9_0 - (aad_9 * 2.46557)
end if

  end subroutine bands

end program BasketballProphet_AI
