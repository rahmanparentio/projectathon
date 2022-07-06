pragma solidity ^0.4.8; //0.4.26+

contract admined {
    address public admin;

    function admined() public {
        admin = msg.sender;
    }

    modifier onlyAdmin(){
        require(msg.sender == admin) ;
        _;
    }

    function transferAdminship(address newAdmin) onlyAdmin public {
        admin = newAdmin;
    }

}

contract ERC20 {

    mapping (address => uint256) public balanceOf;
    mapping (address => mapping(address => uint256)) public allowance;

    string public standard = "Telkom V.1";
    string public name =  "Telkom Athoin COin";
    string public symbol = "TAO";
    uint256 public totalSupply = 1000;

    event Transfer (address indexed from, address indexed to, uint256 value);

    function Input_beredar(uint256 initialSupply) public {
        balanceOf[msg.sender] = initialSupply;
        totalSupply = initialSupply;
    }

    function Topup(address _to, uint256 _tambah) public {
        balanceOf[_to] += _tambah;
    }

    function transfer(address _to, uint256 _value) public {
        require(balanceOf[msg.sender] > _value); // meminta require balanceOf msgsender harus lebih besar dari value yg dimasukkan
        require(balanceOf[_to] + _value > balanceOf[_to]); //  securing line (balanceof tujuan ditambah value transfer > dari current balanceOf tujuan)
       
        balanceOf[msg.sender] -= _value; // balance di msgsender(alamat) berkurang/sama karena value
        balanceOf[_to] += _value; // balance to (tujuan) bertambah/sama karena value
        Transfer(msg.sender, _to, _value); // perpindahan dari msg.sender ke to (tujuan) sejumlah value
    }

    function approval (address _spender, uint256 _value) public returns (bool success) {
        allowance[msg.sender][_spender] = _value;
        return true;
    } 
    

    function transferForm(address _from, address _to, uint256 _value) public returns (bool success) {
        require (balanceOf[_from] > _value); // balanceOf pengirim(from) lebih besar dari value
        require (balanceOf[_to] + _value > balanceOf[_to]); // balanceOf tujuan ditambah value akan lebih besar dari balanceOf awal
        require (_value < allowance[_from][msg.sender]); // 
        balanceOf[_from] -= _value; // balanceOf pengirim(from) akan berkurang sejumlah value 
        balanceOf[_to] += _value; // balanceOf tujuan(to) akan bertambah sejumlah value
        allowance[_from][msg.sender] -= _value;
        Transfer(_from, _to, _value);
        return true;
    }

    function burnFrom(address _from, uint256 _value) public returns (bool success) {
        require(balanceOf[_from] >= _value);                
        require(_value <= allowance[_from][msg.sender]);    // bisa dihapus require nya
        balanceOf[_from] -= _value;                         
        allowance[_from][msg.sender] -= _value;             
        totalSupply -= _value;                              

        return true;
    }
    


}
