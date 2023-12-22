! BSD 3-Clause No Military License
! Copyright © 2023-present, Piotr Bajdek

program BasketballProphet_AI

  character(len=256) :: arg_1
  character(256) :: line
  integer, parameter :: dp = selected_real_kind(15, 307)
  real(dp), dimension(:,:), allocatable :: attention_weights_3
  real(dp), dimension(:,:), allocatable :: attention_weights_4
  real(dp), dimension(:,:), allocatable :: attention_weights_7
  real(dp), dimension(:,:), allocatable :: attention_weights_8
  real(dp), dimension(:), allocatable :: sequence_1, sequence_2
  real(dp), dimension(:), allocatable :: sequence
  real(dp), dimension(3) :: w_3, dw_3
  real(dp), dimension(4) :: w_4, dw_4
  real(dp), dimension(7) :: w_7, dw_7
  real(dp), dimension(8) :: w_8, dw_8
  real(dp) :: b_3 = 0.0_dp, alpha_3 = 0.001_dp, beta1_3 = 0.9_dp, beta2_3 = 0.999_dp, t_3 = 0.0_dp
  real(dp) :: b_4 = 0.0_dp, alpha_4 = 0.001_dp, beta1_4 = 0.9_dp, beta2_4 = 0.999_dp, t_4 = 0.0_dp
  real(dp) :: b_7 = 0.0_dp, alpha_7 = 0.001_dp, beta1_7 = 0.9_dp, beta2_7 = 0.999_dp, t_7 = 0.0_dp
  real(dp) :: b_8 = 0.0_dp, alpha_8 = 0.001_dp, beta1_8 = 0.9_dp, beta2_8 = 0.999_dp, t_8 = 0.0_dp
  real(dp) :: mt_3(3) = 0.0_dp, vt_3(3) = 0.0_dp, m_hat_3(3), v_hat_3(3), denom_3(3)
  real(dp) :: mt_4(4) = 0.0_dp, vt_4(4) = 0.0_dp, m_hat_4(4), v_hat_4(4), denom_4(4)
  real(dp) :: mt_7(7) = 0.0_dp, vt_7(7) = 0.0_dp, m_hat_7(7), v_hat_7(7), denom_7(7)
  real(dp) :: mt_8(8) = 0.0_dp, vt_8(8) = 0.0_dp, m_hat_8(8), v_hat_8(8), denom_8(8)
  real(dp) :: db_3, y_pred_3, error_3, db_4, y_pred_4, error_4
  real(dp) :: db_7, y_pred_7, error_7, db_8, y_pred_8, error_8
  real(dp) :: epsilon = 1.0E-8_dp
  integer :: i, j, iostat, seq_len, iterations = 12500
  real :: result_1_3, result_2_3, result_1_4, result_2_4
  real :: result_1_7, result_2_7, result_1_8, result_2_8
  real :: result_1, result_2

  character(len=4) :: reset = ''//achar(27)//'[0m'
  character(len=5) :: red = ''//achar(27)//'[31m'
  character(len=5) :: green = ''//achar(27)//'[32m'
  character(len=5) :: brown = ''//achar(27)//'[33m'
  character(len=11) :: grey = ''//achar(27)//'[38;5;246m'

  character(len=5) :: a_col_3, b_col_3, a_col_4, b_col_4
  character(len=5) :: a_col_7, b_col_7, a_col_8, b_col_8
  character(len=5) :: a_col, b_col

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
    call ai_3()
  result_1_3 = y_pred_3
    deallocate(attention_weights_3)

  sequence = sequence_2
    call ai_3()
  result_2_3 = y_pred_3
    deallocate(attention_weights_3)

  if (result_1_3 > result_2_3) then
    a_col_3 = green
    b_col_3 = red
  else if (result_1_3 < result_2_3) then
    a_col_3 = red
    b_col_3 = green
  else
    a_col_3 = brown
    b_col_3 = brown
  end if

  sequence = sequence_1
    call ai_4()
  result_1_4 = y_pred_4
    deallocate(attention_weights_4)

  sequence = sequence_2
    call ai_4()
  result_2_4 = y_pred_4
    deallocate(attention_weights_4)

  if (result_1_4 > result_2_4) then
    a_col_4 = green
    b_col_4 = red
  else if (result_1_4 < result_2_4) then
    a_col_4 = red
    b_col_4 = green
  else
    a_col_4 = brown
    b_col_4 = brown
  end if

  sequence = sequence_1
    call ai_7()
  result_1_7 = y_pred_7
    deallocate(attention_weights_7)

  sequence = sequence_2
    call ai_7()
  result_2_7 = y_pred_7
    deallocate(attention_weights_7)

  if (result_1_7 > result_2_7) then
    a_col_7 = green
    b_col_7 = red
  else if (result_1_7 < result_2_7) then
    a_col_7 = red
    b_col_7 = green
  else
    a_col_7 = brown
    b_col_7 = brown
  end if

  sequence = sequence_1
    call ai_8()
  result_1_8 = y_pred_8
    deallocate(sequence_1, attention_weights_8)

  sequence = sequence_2
    call ai_8()
  result_2_8 = y_pred_8
    deallocate(sequence_2, attention_weights_8)

  if (result_1_8 > result_2_8) then
    a_col_8 = green
    b_col_8 = red
  else if (result_1_8 < result_2_8) then
    a_col_8 = red
    b_col_8 = green
  else
    a_col_8 = brown
    b_col_8 = brown
  end if

  result_1 = (result_1_3 + result_1_4 + result_1_7 + result_1_8) / 4
  result_2 = (result_2_3 + result_2_4 + result_2_7 + result_2_8) / 4

  if (result_1 > result_2) then
    a_col = green
    b_col = red
  else if (result_1 < result_2) then
    a_col = red
    b_col = green
  else
    a_col = brown
    b_col = brown
  end if

  write(*,'(a)') 'Neural Network-Based Sport Predictions'
  write(*,'(a,a,a,a,a)') grey, 'BasketballProphet AI', reset, ' v0.3.0 22.12.2023'
  write(*,'(a)') 'Copyright © 2023-present, Piotr Bajdek'
  print *,''
  write(*,'(a,a,a,a)') 'Loaded file ', grey, arg_1, reset
  write(*,'(a,a,I4,a,a,a,I4,a,a)') 'Next score:', a_col_3, nint(result_1_3), reset, ' :', b_col_3, nint(result_2_3), reset, &
  ' (3 last scs.) 🏀'
  write(*,'(a,a,I4,a,a,a,I4,a,a)') 'Next score:', a_col_4, nint(result_1_4), reset, ' :', b_col_4, nint(result_2_4), reset, &
  ' (4 last scs.) 🏀'
  write(*,'(a,a,I4,a,a,a,I4,a,a)') 'Next score:', a_col_7, nint(result_1_7), reset, ' :', b_col_7, nint(result_2_7), reset, &
  ' (7 last scs.) 🏀'
  write(*,'(a,a,I4,a,a,a,I4,a,a)') 'Next score:', a_col_8, nint(result_1_8), reset, ' :', b_col_8, nint(result_2_8), reset, &
  ' (8 last scs.) 🏀'
  print *,''
  write(*,'(a,a,I4,a,a,a,I4,a,a)') 'Next score:', a_col, nint(result_1), reset, ' :', b_col, nint(result_2), reset, &
  ' avg. predict. 🏀'

contains

  subroutine ai_3()

  w_3 = 0.0_dp

  allocate(attention_weights_3(seq_len, seq_len))
  attention_weights_3 = 0.0_dp

  do i=1, iterations

      dw_3 = 0.0_dp

      do j = 1, seq_len
        attention_weights_3(:, j) = sequence(j) * sequence(:)
      end do

      attention_weights_3 = attention_weights_3 / real(seq_len)

      do j=4, seq_len
          y_pred_3 = w_3(1) * sequence(j-2) &
          + w_3(2) * sequence(j-3) &
          + w_3(3) * sequence(j-4) &
          + b_3
          error_3 = y_pred_3 - sequence(j)
          dw_3(1) = dw_3(1) + error_3 * sequence(j-2) * attention_weights_3(j-2, j)
          dw_3(2) = dw_3(2) + error_3 * sequence(j-3) * attention_weights_3(j-3, j)
          dw_3(3) = dw_3(3) + error_3 * sequence(j-4) * attention_weights_3(j-4, j)
          db_3 = db_3 + error_3
      end do

      dw_3 = dw_3 / (seq_len - 3)
      db_3 = db_3 / (seq_len - 3)

      t_3 = t_3 + 1.0_dp
      mt_3 = beta1_3 * mt_3 + (1.0_dp - beta1_3) * dw_3
      vt_3 = beta2_3 * vt_3 + (1.0_dp - beta2_3) * (dw_3**2)
      m_hat_3 = mt_3 / (1.0_dp - beta1_3**t_3)
      v_hat_3 = vt_3 / (1.0_dp - beta2_3**t_3)
      denom_3 = sqrt(v_hat_3) + epsilon
      w_3 = w_3 - alpha_3 * m_hat_3 / denom_3
      b_3 = b_3 - alpha_3 * db_3

  end do

  y_pred_3 = w_3(1) * sequence(seq_len-2) &
      + w_3(2) * sequence(seq_len-3) &
      + w_3(3) * sequence(seq_len-4) &
      + b_3

  b_3 = 0.0
  db_3 = 0.0

  end subroutine ai_3

  subroutine ai_4()

  w_4 = 0.0_dp

  allocate(attention_weights_4(seq_len, seq_len))
  attention_weights_4 = 0.0_dp

  do i=1, iterations

      dw_4 = 0.0_dp

      do j = 1, seq_len
        attention_weights_4(:, j) = sequence(j) * sequence(:)
      end do

      attention_weights_4 = attention_weights_4 / real(seq_len)

      do j=5, seq_len
          y_pred_4 = w_4(1) * sequence(j-2) &
          + w_4(2) * sequence(j-3) &
          + w_4(3) * sequence(j-4) &
          + w_4(4) * sequence(j-5) &
          + b_4
          error_4 = y_pred_4 - sequence(j)
          dw_4(1) = dw_4(1) + error_4 * sequence(j-2) * attention_weights_4(j-2, j)
          dw_4(2) = dw_4(2) + error_4 * sequence(j-3) * attention_weights_4(j-3, j)
          dw_4(3) = dw_4(3) + error_4 * sequence(j-4) * attention_weights_4(j-4, j)
          dw_4(4) = dw_4(4) + error_4 * sequence(j-5) * attention_weights_4(j-5, j)
          db_4 = db_4 + error_4
      end do

      dw_4 = dw_4 / (seq_len - 4)
      db_4 = db_4 / (seq_len - 4)

      t_4 = t_4 + 1.0_dp
      mt_4 = beta1_4 * mt_4 + (1.0_dp - beta1_4) * dw_4
      vt_4 = beta2_4 * vt_4 + (1.0_dp - beta2_4) * (dw_4**2)
      m_hat_4 = mt_4 / (1.0_dp - beta1_4**t_4)
      v_hat_4 = vt_4 / (1.0_dp - beta2_4**t_4)
      denom_4 = sqrt(v_hat_4) + epsilon
      w_4 = w_4 - alpha_4 * m_hat_4 / denom_4
      b_4 = b_4 - alpha_4 * db_4

  end do

  y_pred_4 = w_4(1) * sequence(seq_len-2) &
      + w_4(2) * sequence(seq_len-3) &
      + w_4(3) * sequence(seq_len-4) &
      + w_4(4) * sequence(seq_len-5) &
      + b_4

  b_4 = 0.0
  db_4 = 0.0

  end subroutine ai_4

  subroutine ai_7()

  w = 0.0_dp

  allocate(attention_weights_7(seq_len, seq_len))
  attention_weights_7 = 0.0_dp

  do i=1, iterations

      dw_7 = 0.0_dp

      do j = 1, seq_len
        attention_weights_7(:, j) = sequence(j) * sequence(:)
      end do

      attention_weights_7 = attention_weights_7 / real(seq_len)

      do j=8, seq_len
          y_pred_7 = w_7(1) * sequence(j-2) &
          + w_7(2) * sequence(j-3) &
          + w_7(3) * sequence(j-4) &
          + w_7(4) * sequence(j-5) &
          + w_7(5) * sequence(j-6) &
          + w_7(6) * sequence(j-7) &
          + w_7(7) * sequence(j-8) &
          + b_7
          error_7 = y_pred_7 - sequence(j)
          dw_7(1) = dw_7(1) + error_7 * sequence(j-2) * attention_weights_7(j-2, j)
          dw_7(2) = dw_7(2) + error_7 * sequence(j-3) * attention_weights_7(j-3, j)
          dw_7(3) = dw_7(3) + error_7 * sequence(j-4) * attention_weights_7(j-4, j)
          dw_7(4) = dw_7(4) + error_7 * sequence(j-5) * attention_weights_7(j-5, j)
          dw_7(5) = dw_7(5) + error_7 * sequence(j-6) * attention_weights_7(j-6, j)
          dw_7(6) = dw_7(6) + error_7 * sequence(j-7) * attention_weights_7(j-7, j)
          dw_7(7) = dw_7(7) + error_7 * sequence(j-8) * attention_weights_7(j-8, j)
          db_7 = db_7 + error_7
      end do

      dw_7 = dw_7 / (seq_len - 7)
      db_7 = db_7 / (seq_len - 7)

      t_7 = t_7 + 1.0_dp
      mt_7 = beta1_7 * mt_7 + (1.0_dp - beta1_7) * dw_7
      vt_7 = beta2_7 * vt_7 + (1.0_dp - beta2_7) * (dw_7**2)
      m_hat_7 = mt_7 / (1.0_dp - beta1_7**t_7)
      v_hat_7 = vt_7 / (1.0_dp - beta2_7**t_7)
      denom_7 = sqrt(v_hat_7) + epsilon
      w_7 = w_7 - alpha_7 * m_hat_7 / denom_7
      b_7 = b_7 - alpha_7 * db_7

  end do

  y_pred_7 = w_7(1) * sequence(seq_len-2) &
      + w_7(2) * sequence(seq_len-3) &
      + w_7(3) * sequence(seq_len-4) &
      + w_7(4) * sequence(seq_len-5) &
      + w_7(5) * sequence(seq_len-6) &
      + w_7(6) * sequence(seq_len-7) &
      + w_7(7) * sequence(seq_len-8) &
      + b_7

  b_7 = 0.0
  db_7 = 0.0

  end subroutine ai_7

  subroutine ai_8()

  w = 0.0_dp

  allocate(attention_weights_8(seq_len, seq_len))
  attention_weights_8 = 0.0_dp

  do i=1, iterations

      dw_8 = 0.0_dp

      do j = 1, seq_len
        attention_weights_8(:, j) = sequence(j) * sequence(:)
      end do

      attention_weights_8 = attention_weights_8 / real(seq_len)

      do j=9, seq_len
          y_pred_8 = w_8(1) * sequence(j-2) &
          + w_8(2) * sequence(j-3) &
          + w_8(3) * sequence(j-4) &
          + w_8(4) * sequence(j-5) &
          + w_8(5) * sequence(j-6) &
          + w_8(6) * sequence(j-7) &
          + w_8(7) * sequence(j-8) &
          + w_8(8) * sequence(j-9) &
          + b_8
          error_8 = y_pred_8 - sequence(j)
          dw_8(1) = dw_8(1) + error_8 * sequence(j-2) * attention_weights_8(j-2, j)
          dw_8(2) = dw_8(2) + error_8 * sequence(j-3) * attention_weights_8(j-3, j)
          dw_8(3) = dw_8(3) + error_8 * sequence(j-4) * attention_weights_8(j-4, j)
          dw_8(4) = dw_8(4) + error_8 * sequence(j-5) * attention_weights_8(j-5, j)
          dw_8(5) = dw_8(5) + error_8 * sequence(j-6) * attention_weights_8(j-6, j)
          dw_8(6) = dw_8(6) + error_8 * sequence(j-7) * attention_weights_8(j-7, j)
          dw_8(7) = dw_8(7) + error_8 * sequence(j-8) * attention_weights_8(j-8, j)
          dw_8(8) = dw_8(8) + error_8 * sequence(j-9) * attention_weights_8(j-9, j)
          db_8 = db_8 + error_8
      end do

      dw_8 = dw_8 / (seq_len - 8)
      db_8 = db_8 / (seq_len - 8)

      t_8 = t_8 + 1.0_dp
      mt_8 = beta1_8 * mt_8 + (1.0_dp - beta1_8) * dw_8
      vt_8 = beta2_8 * vt_8 + (1.0_dp - beta2_8) * (dw_8**2)
      m_hat_8 = mt_8 / (1.0_dp - beta1_8**t_8)
      v_hat_8 = vt_8 / (1.0_dp - beta2_8**t_8)
      denom_8 = sqrt(v_hat_8) + epsilon
      w_8 = w_8 - alpha_8 * m_hat_8 / denom_8
      b_8 = b_8 - alpha_8 * db_8

  end do

  y_pred_8 = w_8(1) * sequence(seq_len-2) &
      + w_8(2) * sequence(seq_len-3) &
      + w_8(3) * sequence(seq_len-4) &
      + w_8(4) * sequence(seq_len-5) &
      + w_8(5) * sequence(seq_len-6) &
      + w_8(6) * sequence(seq_len-7) &
      + w_8(7) * sequence(seq_len-8) &
      + w_8(8) * sequence(seq_len-9) &
      + b_8

  b_8 = 0.0
  db_8 = 0.0

  end subroutine ai_8

end program BasketballProphet_AI
