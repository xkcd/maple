import React, {useCallback} from 'react'

type InputProps = {
  onChangeValue: (value: string) => void,
} & React.HTMLProps<HTMLInputElement>

const Input: React.FC<InputProps> = ({onChangeValue, ...props}) => {
  const handleChange = useCallback((ev: React.ChangeEvent<HTMLInputElement>) => {
    onChangeValue(ev.target.value)
  }, [])
  return <input onChange={handleChange} {...props} />
}

export default Input
