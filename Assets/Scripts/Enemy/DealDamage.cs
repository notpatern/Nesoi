using UnityEngine;

public class DealDamage : MonoBehaviour , IDamageable
{
    public Rigidbody rb;
    [SerializeField] private int damage;
    public Transform parent;
    public bool canDamage;

    private void Start()
    {
        canDamage = false;
        rb.useGravity = false;
    }

    private void OnTriggerEnter(Collider other)
    {
        var target = other.GetComponent<IDamageable>();
        if (other.CompareTag("Enemy") && !canDamage) return;
        if (target != null) {
            target.TakeDamage(damage);
            Destroy(gameObject);
        }
        if (other.CompareTag("Terrain")) Destroy(gameObject);
    }

    public bool CanDamage()
    {  
        return canDamage;
    }

    public void TakeDamage(int damage)
    {
        
    }

    public int Damage()
    {
        return damage;
    }
}
