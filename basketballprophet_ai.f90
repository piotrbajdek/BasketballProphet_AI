! BSD 3-Clause No Military License
! Copyright © 2023-present, Piotr Bajdek

program BasketballProphet_AI

  character(len=256) :: arg_1
  character(256) :: line
  integer, parameter :: dp = selected_real_kind(15, 307)
  real(dp), dimension(:,:), allocatable :: attention_weights
  real(dp), dimension(:), allocatable :: sequence
  real(dp), dimension(:), allocatable :: sequence_1, sequence_2
  real(dp), dimension(8) :: w, dw
  real(dp) :: b = 0.0_dp, alpha = 0.001_dp, beta1 = 0.9_dp, beta2 = 0.999_dp, t = 0.0_dp
  real(dp) :: mt(8) = 0.0_dp, vt(8) = 0.0_dp, m_hat(8), v_hat(8), denom(8)
  real(dp) :: db, y_pred, error
  real(dp) :: epsilon = 1.0E-8_dp
  integer :: i, j, iostat, iterations = 12500, seq_len
  real :: result_1, result_2

  character(len=4) :: reset = ''//achar(27)//'[0m'
  character(len=5) :: red = ''//achar(27)//'[31m'
  character(len=5) :: green = ''//achar(27)//'[32m'
  character(len=5) :: brown = ''//achar(27)//'[33m'
  character(len=11) :: grey = ''//achar(27)//'[38;5;246m'

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
    call ai()
  result_1 = y_pred
    deallocate(sequence_1, attention_weights)

  sequence = sequence_2
    call ai()
  result_2 = y_pred
    deallocate(sequence_2, attention_weights)

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
  write(*,'(a,a,a,a,a)') grey, 'BasketballProphet AI', reset, ' v0.1.1 11.12.2023'
  write(*,'(a)') 'Copyright © 2023-present, Piotr Bajdek'
  print *,''
  write(*,'(a,a,a,a)') 'Loaded file ', grey, arg_1, reset
  write(*,'(a,a,I4,a,a,a,I4,a,a)') 'Next score:', a_col, nint(result_1), reset, ' :', b_col, nint(result_2), reset, &
  '               🏀'

contains

  subroutine ai()

  w = [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp]

  allocate(attention_weights(seq_len, seq_len))
  attention_weights = 0.0_dp

  do i=1, iterations

      dw = [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp]

      do j = 1, seq_len
        attention_weights(:, j) = sequence(j) * sequence(:)
      end do

      attention_weights = attention_weights / real(seq_len)

      do j=9, seq_len
          y_pred = w(1) * sequence(j-2) &
          + w(2) * sequence(j-3) &
          + w(3) * sequence(j-4) &
          + w(4) * sequence(j-5) &
          + w(5) * sequence(j-6) &
          + w(6) * sequence(j-7) &
          + w(7) * sequence(j-8) &
          + w(8) * sequence(j-9) &
          + b
          error = y_pred - sequence(j)
          dw(1) = dw(1) + error * sequence(j-2) * attention_weights(j-2, j)
          dw(2) = dw(2) + error * sequence(j-3) * attention_weights(j-3, j)
          dw(3) = dw(3) + error * sequence(j-4) * attention_weights(j-4, j)
          dw(4) = dw(4) + error * sequence(j-5) * attention_weights(j-5, j)
          dw(5) = dw(5) + error * sequence(j-6) * attention_weights(j-6, j)
          dw(6) = dw(6) + error * sequence(j-7) * attention_weights(j-7, j)
          dw(7) = dw(7) + error * sequence(j-8) * attention_weights(j-8, j)
          dw(8) = dw(8) + error * sequence(j-9) * attention_weights(j-9, j)
          db = db + error
      end do

      dw = dw / (seq_len - 8)
      db = db / (seq_len - 8)

      t = t + 1.0_dp
      mt = beta1 * mt + (1.0_dp - beta1) * dw
      vt = beta2 * vt + (1.0_dp - beta2) * (dw**2)
      m_hat = mt / (1.0_dp - beta1**t)
      v_hat = vt / (1.0_dp - beta2**t)
      denom = sqrt(v_hat) + epsilon
      w = w - alpha * m_hat / denom
      b = b - alpha * db

  end do

  y_pred = w(1) * sequence(seq_len-2) &
      + w(2) * sequence(seq_len-3) &
      + w(3) * sequence(seq_len-4) &
      + w(4) * sequence(seq_len-5) &
      + w(5) * sequence(seq_len-6) &
      + w(6) * sequence(seq_len-7) &
      + w(7) * sequence(seq_len-8) &
      + w(8) * sequence(seq_len-9) &
      + b

  b = 0.0
  db = 0.0

  end subroutine ai

end program BasketballProphet_AI
